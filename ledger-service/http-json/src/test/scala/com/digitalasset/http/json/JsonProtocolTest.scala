// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.http.json

import com.digitalasset.http.Generators.{
  OptionalPackageIdGen,
  contractGen,
  contractLocatorGen,
  genDomainTemplateId,
  genDomainTemplateIdO
}
import com.digitalasset.http.domain
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Inside, Matchers}
import scalaz.{\/, \/-}
import scalaz.syntax.std.option._

class JsonProtocolTest
    extends FreeSpec
    with Matchers
    with Inside
    with GeneratorDrivenPropertyChecks {

  import JsonProtocol._
  import spray.json._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "domain.TemplateId.RequiredPkg" - {
    "can be serialized to JSON" in forAll(genDomainTemplateId) { a: domain.TemplateId.RequiredPkg =>
      inside(a.toJson) {
        case JsString(str) =>
          str should ===(s"${a.packageId}:${a.moduleName}:${a.entityName}")
      }
    }
    "roundtrips" in forAll(genDomainTemplateId) { a: domain.TemplateId.RequiredPkg =>
      val b = a.toJson.convertTo[domain.TemplateId.RequiredPkg]
      b should ===(a)
    }
  }

  "domain.TemplateId.OptionalPkg" - {
    "can be serialized to JSON" in forAll(genDomainTemplateIdO(OptionalPackageIdGen)) {
      a: domain.TemplateId.OptionalPkg =>
        val expectedStr: String = a.packageId.cata(
          p => s"${p: String}:${a.moduleName}:${a.entityName}",
          s"${a.moduleName}:${a.entityName}")

        inside(a.toJson) {
          case JsString(str) =>
            str should ===(expectedStr)
        }
    }
    "roundtrips" in forAll(genDomainTemplateIdO) { a: domain.TemplateId.OptionalPkg =>
      val b = a.toJson.convertTo[domain.TemplateId.OptionalPkg]
      b should ===(a)
    }
  }

  "domain.Contract" - {
    "can be serialized to JSON" in forAll(contractGen) { contract =>
      inside(SprayJson.encode(contract)) {
        case \/-(JsObject(fields)) =>
          inside(fields.toList) {
            case List(("archived", JsObject(_))) =>
            case List(("created", JsObject(_))) =>
          }
      }
    }
    "can be serialized and deserialized back to the same object" in forAll(contractGen) {
      contract0 =>
        val actual: SprayJson.Error \/ domain.Contract[JsValue] = for {
          jsValue <- SprayJson.encode(contract0)
          contract <- SprayJson.decode[domain.Contract[JsValue]](jsValue)
        } yield contract

        inside(actual) {
          case \/-(contract1) => contract1 shouldBe contract0
        }
    }
  }

  "domain.ContractLocator" - {
    type Loc = domain.ContractLocator[JsValue]
    "roundtrips" in forAll(contractLocatorGen(arbitrary[Int] map (JsNumber(_)))) { locator: Loc =>
      locator.toJson.convertTo[Loc] should ===(locator)
    }
  }
}
