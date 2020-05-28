// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.validator

import com.daml.caching.{Cache, Configuration}
import com.daml.ledger.participant.state.kvutils.DamlKvutils.{DamlStateKey, DamlStateValue}
import com.daml.ledger.participant.state.kvutils.caching.`Message Weight`
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{AsyncWordSpec, Inside, Matchers}

import scala.concurrent.{ExecutionContext, Future}

class CachingDamlLedgerStateReaderSpec
    extends AsyncWordSpec
    with Matchers
    with Inside
    with MockitoSugar {

  private val keySerializationStrategy = DefaultStateKeySerializationStrategy

  "readState" should {
    "record read keys" in {
      val mockReader = mock[DamlLedgerStateReader]
      when(mockReader.readState(argThat((keys: Seq[DamlStateKey]) => keys.size == 1)))
        .thenReturn(Future.successful(Seq(Some(aDamlStateValue()))))
      val instance = newInstance(mockReader)

      instance.readState(Seq(aDamlStateKey)).map { actual =>
        actual should have size 1
        instance.getReadSet should be(
          Set(keySerializationStrategy.serializeStateKey(aDamlStateKey)))
      }
    }

    "write to cache what was read" in {
      val mockReader = mock[DamlLedgerStateReader]
      when(mockReader.readState(argThat((keys: Seq[DamlStateKey]) => keys.size == 1)))
        .thenReturn(Future.successful(Seq(Some(aDamlStateValue()))))
      val instance = newInstance(mockReader)

      instance.readState(Seq(aDamlStateKey)).map { _ =>
        instance.cache.getIfPresent(aDamlStateKey) should not be null
      }
    }

    "serve request from cache for seen key" in {
      val mockReader = mock[DamlLedgerStateReader]
      when(mockReader.readState(any[Seq[DamlStateKey]]())).thenReturn(Future.successful(Seq(None)))
      val instance = newInstance(mockReader)

      for {
        originalReadState <- instance.readState(Seq(aDamlStateKey))
        readAgain <- instance.readState(Seq(aDamlStateKey))
      } yield {
        verify(mockReader, times(1)).readState(_)
        readAgain shouldEqual originalReadState
      }
    }
  }

  private lazy val aDamlStateKey = DamlStateKey.newBuilder
    .setContractId("aContractId")
    .build

  private def aDamlStateValue(): DamlStateValue = DamlStateValue.getDefaultInstance

  private def newInstance(damlLedgerStateReader: DamlLedgerStateReader)(
      implicit executionContext: ExecutionContext): CachingDamlLedgerStateReader = {
    val cache = Cache.from[DamlStateKey, DamlStateValue](Configuration(1024))
    new CachingDamlLedgerStateReader(cache, keySerializationStrategy, damlLedgerStateReader)
  }
}