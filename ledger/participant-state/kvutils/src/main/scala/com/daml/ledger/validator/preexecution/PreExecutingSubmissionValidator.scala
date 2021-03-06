// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.validator.preexecution

import com.daml.ledger.participant.state.kvutils.DamlKvutils.{
  DamlStateKey,
  DamlStateValue,
  DamlSubmission
}
import com.daml.ledger.participant.state.kvutils.api.LedgerReader
import com.daml.ledger.participant.state.kvutils.{
  Bytes,
  DamlStateMapWithFingerprints,
  Envelope,
  Fingerprint,
  KeyValueCommitting
}
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.ValidationFailed
import com.daml.ledger.validator.batch.BatchedSubmissionValidator
import com.daml.ledger.validator.preexecution.PreExecutingSubmissionValidator._
import com.daml.ledger.validator.reading.StateReader
import com.daml.logging.{ContextualizedLogger, LoggingContext}
import com.daml.metrics.{Metrics, Timed}

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}

/**
  * Validator for pre-executing submissions.
  */
class PreExecutingSubmissionValidator[ReadSet, WriteSet](
    committer: KeyValueCommitting,
    metrics: Metrics,
    commitStrategy: PreExecutingCommitStrategy[
      DamlStateKey,
      (Option[DamlStateValue], Fingerprint),
      ReadSet,
      WriteSet,
    ],
) {
  private val logger = ContextualizedLogger.get(getClass)

  def validate(
      submissionEnvelope: Bytes,
      submittingParticipantId: ParticipantId,
      ledgerStateReader: DamlLedgerStateReaderWithFingerprints,
  )(
      implicit executionContext: ExecutionContext,
      loggingContext: LoggingContext,
  ): Future[PreExecutionOutput[ReadSet, WriteSet]] =
    Timed.timedAndTrackedFuture(
      metrics.daml.kvutils.submission.validator.validatePreExecute,
      metrics.daml.kvutils.submission.validator.validatePreExecuteRunning,
      for {
        decodedSubmission <- decodeSubmission(submissionEnvelope)
        fetchedInputs <- fetchSubmissionInputs(decodedSubmission, ledgerStateReader)
        inputState = fetchedInputs.mapValues(_._1)
        preExecutionResult = committer.preExecuteSubmission(
          LedgerReader.DefaultConfiguration,
          decodedSubmission,
          submittingParticipantId,
          inputState,
        )
        logEntryId = BatchedSubmissionValidator.bytesToLogEntryId(submissionEnvelope)
        generatedWriteSets <- Timed.future(
          metrics.daml.kvutils.submission.validator.generateWriteSets,
          commitStrategy.generateWriteSets(
            submittingParticipantId,
            logEntryId,
            fetchedInputs,
            preExecutionResult,
          )
        )
      } yield {
        PreExecutionOutput(
          minRecordTime = preExecutionResult.minimumRecordTime.map(_.toInstant),
          maxRecordTime = preExecutionResult.maximumRecordTime.map(_.toInstant),
          successWriteSet = generatedWriteSets.successWriteSet,
          outOfTimeBoundsWriteSet = generatedWriteSets.outOfTimeBoundsWriteSet,
          readSet = commitStrategy.generateReadSet(fetchedInputs, preExecutionResult.readSet),
          involvedParticipants = generatedWriteSets.involvedParticipants
        )
      }
    )

  private def decodeSubmission(submissionEnvelope: Bytes)(
      implicit executionContext: ExecutionContext,
      loggingContext: LoggingContext,
  ): Future[DamlSubmission] =
    Timed.timedAndTrackedFuture(
      metrics.daml.kvutils.submission.validator.decode,
      metrics.daml.kvutils.submission.validator.decodeRunning,
      Future {
        Envelope.open(submissionEnvelope) match {
          case Right(Envelope.SubmissionMessage(submission)) =>
            metrics.daml.kvutils.submission.validator.receivedSubmissionBytes
              .update(submission.getSerializedSize)
            submission

          case Right(Envelope.SubmissionBatchMessage(_)) =>
            logger.error("Batched submissions are not supported for pre-execution")
            throw ValidationFailed.ValidationError(
              "Batched submissions are not supported for pre-execution")

          case Right(other) =>
            throw ValidationFailed.ValidationError(
              s"Unexpected message in envelope: ${other.getClass.getSimpleName}")

          case Left(error) =>
            throw ValidationFailed.ValidationError(s"Cannot open envelope: $error")
        }
      }
    )

  private def fetchSubmissionInputs(
      submission: DamlSubmission,
      ledgerStateReader: DamlLedgerStateReaderWithFingerprints,
  )(implicit executionContext: ExecutionContext): Future[DamlStateMapWithFingerprints] = {
    val inputKeys = submission.getInputDamlStateList.asScala
    Timed.timedAndTrackedFuture(
      metrics.daml.kvutils.submission.validator.fetchInputs,
      metrics.daml.kvutils.submission.validator.fetchInputsRunning,
      for {
        inputValues <- ledgerStateReader.read(inputKeys)

        nestedInputKeys = inputValues.collect {
          case (Some(value), _) if value.hasContractKeyState =>
            val contractId = value.getContractKeyState.getContractId
            DamlStateKey.newBuilder.setContractId(contractId).build
        }
        nestedInputValues <- ledgerStateReader.read(nestedInputKeys)
      } yield {
        assert(inputKeys.size == inputValues.size)
        assert(nestedInputKeys.size == nestedInputValues.size)
        val inputPairs = inputKeys.toIterator zip inputValues.toIterator
        val nestedInputPairs = nestedInputKeys.toIterator zip nestedInputValues.toIterator
        (inputPairs ++ nestedInputPairs).toMap
      }
    )
  }
}

object PreExecutingSubmissionValidator {
  type DamlLedgerStateReaderWithFingerprints =
    StateReader[DamlStateKey, (Option[DamlStateValue], Fingerprint)]
}
