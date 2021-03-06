// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package transaction

import value.ValueVersion
import com.daml.lf.language.LanguageVersion
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TransactionVersionSpec extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {

  import LanguageVersion.{v1_6, v1_7, v1_8, v1_dev}
  import TransactionVersion.{v10, vDev}

  "TransactionVersion.assignNodeVersion" should {

    val testCases = Table(
      "language version" -> "transaction version",
      v1_6 -> v10,
      v1_7 -> v10,
      v1_8 -> v10,
      v1_dev -> vDev,
    )

    "be stable" in {
      forEvery(testCases) { (languageVersion, transactionVersions) =>
        TransactionVersion.assignNodeVersion(languageVersion) shouldBe transactionVersions
      }
    }

  }

  "TransactionVersion.assignValueVersion" should {
    "be stable" in {

      val testCases = Table(
        "input" -> "output",
        v10 -> ValueVersion("6"),
        vDev -> ValueVersion("dev")
      )

      forEvery(testCases) { (input, expectedOutput) =>
        TransactionVersion.assignValueVersion(input) shouldBe expectedOutput
      }

    }
  }

}
