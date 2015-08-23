## Json4Scala
Json4Scala is a one-file-only JSON library for Scala.
The file, "json.scala", has no more than 300 lines.
Json Objects can be easy designed, benefited from the "Any" class of Scala. Array was implemented by "Array[Value]", and Object was implemented by Map[String, Value].
I was trying to implement the code as short as possible, so you can understand the detail easily.
For more detail, see "json.scala".



### Some Test Case
        
    class alltest extends FunSuite {
        test("base") {
            assert(Json.parse("123").asInt == 123)
            assert(Json.parse("-123").asInt == -123)
            assert(Json.parse("111111111111111").asLong == 111111111111111l)
            assert(Json.parse("true").asBoolean == true)
            assert(Json.parse("false").asBoolean == false)
            assert(Json.parse("123.123").asDouble == 123.123)
            assert(Json.parse("\"aaa\"").asString == "aaa")
            assert(Json.parse("\"aaa\"").write() == "\"aaa\"")
            
            val json = Json.Value(Map("a" -> Array(1,2,3), "b" -> Array(4, 5, 6)))
            assert(json("a")(0).asInt == 1)
            assert(json("b")(1).asInt == 5)
        }
        test("parse base") {
            val str =
                """
                  {"int":-123, "long": 111111111111111, "string":"asdf", "bool_true": true, "foo":"foo", "bool_false": false}
                """
            val json = Json.parse(str)
            assert(json.asMap("int").asInt == -123)
            assert(json.asMap("long").asLong == 111111111111111l)
            assert(json.asMap("string").asString == "asdf")
            assert(json.asMap("bool_true").asBoolean == true)
            assert(json.asMap("bool_false").asBoolean == false)
            println(json.write())
            assert(json.write().length > 0)
        }
        test("parse obj") {
            val str =
                """
                   {"asdf":[1,2,4,{"bbb":"ttt"},432]}
                """
            val json = Json.parse(str)
            assert(json.asMap("asdf").asArray(0).asInt == 1)
            assert(json.asMap("asdf").asArray(3).asMap("bbb").asString == "ttt")
        }
        test("parse array") {
            val str =
                """
                   [1,2,3,4,{"a":[1,2,3]}]
                """
            val json = Json.parse(str)
            assert(json.asArray(0).asInt == 1)
            assert(json(4)("a")(2).asInt == 3)
            assert(json(4)("a")(2).isInt)
            assert(json(4)("a").isArray)
            assert(json(4)("a").isMap == false)
        }
        test("real") {
            val str = "{\"styles\":[214776380871671808,214783111085424640,214851869216866304,214829406537908224],\"group\":100,\"name\":\"AO4614【金宏达电子】现货库存 质量保证 欢迎购买@\",\"shopgrade\":8,\"price\":0.59,\"shop_id\":60095469,\"C3\":50018869,\"C2\":50024099,\"C1\":50008090,\"imguri\":\"http://img.geilicdn.com/taobao10000177139_425x360.jpg\",\"cag\":50006523,\"soldout\":0,\"C4\":50006523}"
            val json = Json.parse(str)
            println(json.write())
            assert(json.asMap.size > 0)
        }
    }
    

### Benchmark
I parsed a text file which has 1000000 json-formatted strings to in-memory Values, and then serialized the Values back to strings and restored them to another text file.

With Json4Scala, the process cost time:

    27.11s user 3.80s system 123% cpu 24.950 total
With [spray-json][1], the process cost time:

    58.44s user 3.43s system 114% cpu 53.826 total
    


  [1]: https://github.com/spray/spray-json
