/**
 * Created by gaoyunxiang on 8/22/15.
 */

import scala.collection.mutable

object Json {

    object Type extends Enumeration {
        val NULL, INT, DOUBLE, BOOLEAN, STRING, ARRAY, OBJECT = Value
    }

    class Value(input_value: Any) {

        def asInt: Int = value match {
            case v: Long if v.toInt == v => v.toInt
        }
        def isInt:Boolean = value.isInstanceOf[Long] || value.isInstanceOf[Int]

        def asLong = value.asInstanceOf[Long]

        def asDouble = value.asInstanceOf[Double]
        def isDouble = value.isInstanceOf[Double]

        def asBoolean = value.asInstanceOf[Boolean]
        def isBoolean = value.isInstanceOf[Boolean]

        def asString = value.asInstanceOf[String]
        def isString = value.isInstanceOf[String]

        def asArray = value.asInstanceOf[Array[Value]]
        def isArray = value.isInstanceOf[Array[_]]

        def asMap = value.asInstanceOf[Map[String, Value]]
        def isMap = value.isInstanceOf[Map[_, _]]

        def apply(i: Int): Value = value.asInstanceOf[Array[Value]](i)

        def apply(key: String): Value = value.asInstanceOf[Map[String, Value]](key)

        def write(): String = {
            val buffer = new mutable.StringBuilder()
            rec_write(buffer)
            buffer.toString()
        }

        private val value: Any = input_value match {
            case null         => null
            case v: Int       => v.toLong
            case v: Long      => input_value
            case v: Double    => input_value
            case v: Boolean   => input_value
            case v: String    => input_value
            case v: Value     => v.value
            case v: Map[_, _] =>
                v.map { case one =>
                    (one._1.toString, Value(one._2))
                }
            case v: Vector[_] => v.map(Value(_)).toArray
            case v: List[_]   => v.map(Value(_)).toArray
            case v: Array[_]  => v.map(Value(_))
            case v: Iterator[_] => v.map(Value(_)).toArray

            case _ => throw new Exception("unknow type")
        }

        private def rec_write(buffer: mutable.StringBuilder): Unit = {
            value match {
                case null        => buffer.append("null")
                case v: Long     => buffer.append(v)
                case v: Boolean  => buffer.append(v)
                case v: Double   => buffer.append(v)
                case v: String   =>
                    buffer.append('"')
                    v.foreach {
                        each => {
                            if (each == '\\' || each == '"') {
                                buffer.append('\\')
                            } else if (each == '\b') {
                                buffer.append("\\b")
                            } else if (each == '\f') {
                                buffer.append("\\f")
                            } else if (each == '\n') {
                                buffer.append("\\n")
                            } else if (each == '\r') {
                                buffer.append("\\r")
                            } else if (each == '\t') {
                                buffer.append("\\t")
                            } else {
                                buffer.append(each)
                            }
                        }
                    }
                    buffer.append('"')
                case v: Array[_] =>
                    buffer.append('[')
                    for (i <- v.indices) {
                        if (i != 0) {
                            buffer.append(',')
                        }
                        v(i).asInstanceOf[Value].rec_write(buffer)
                    }
                    buffer.append(']')
                case v: Map[_, _] =>
                    buffer.append('{')
                    var first = true
                    v.foreach {
                        case one =>
                            if (!first) {
                                buffer.append(',')
                            }
                            first = false
                            buffer.append('"')
                            buffer.append(one._1)
                            buffer.append('"')
                            buffer.append(':')
                            one._2.asInstanceOf[Value].rec_write(buffer)
                    }
                    buffer.append('}')
                case _ => throw new Exception("unknow data type")
            }
        }
    }

    object Value {
        def apply(in: Any): Value = {
            new Value(in)
        }
    }

    def parse(p: String): Value = {
        val sta = mutable.ArrayBuffer[(Char, Any)]()
        var i = 0
        while (i < p.length) {
            if (p(i).isWhitespace || p(i) == '-' || p(i) == ':' || p(i) == ',') {
            } else if (p(i) == '[') {
                sta.append(('[', null))
            } else if (p(i) == '{') {
                sta.append(('{', null))
            } else if (p(i) == ']') {
                val vec = mutable.ArrayStack[Value]()
                while (sta.nonEmpty && sta.last._1 != '[') {
                    vec.push(Value(sta.last._2))
                    sta.trimEnd(1)
                }
                if (sta.isEmpty || sta.last._1 != '[') {
                    throw new Exception("parse error, [] not match")
                }
                sta.trimEnd(1)
                sta.append(('a', vec.iterator))
            } else if (p(i) == '}') {
                val now = mutable.HashMap[String, Value]()

                while (sta.length >= 2 && sta.last._1 != '{') {
                    val new_value: Value = Value(sta.last._2)
                    sta.trimEnd(1)
                    val new_key = sta.last._2.asInstanceOf[String]
                    sta.trimEnd(1)
                    now.update(new_key, new_value)
                }
                if (sta.isEmpty || sta.last._1 != '{') {
                    throw new Exception("parse error, {} not match")
                }
                sta.trimEnd(1)
                sta.append(('o', now.toMap))
            } else if (p(i) == '"') {
                var j = i + 1
                val S = new mutable.StringBuilder()
                while (j < p.length && p(j) != '"') {
                    if (p(j) == '\\') {
                        if (p(j + 1) == 'b') {
                            S.append('\b')
                        } else if (p(j + 1) == 'f') {
                            S.append('\f')
                        } else if (p(j + 1) == 'n') {
                            S.append('\n')
                        } else if (p(j + 1) == 'r') {
                            S.append('\r')
                        } else if (p(j + 1) == 't') {
                            S.append('\t')
                        } else {
                            S.append(p(j + 1))
                        }
                        j += 2
                    } else {
                        S.append(p(j))
                        j += 1
                    }
                }
                sta.append(('v', S.toString()))
                i = j
            } else if (p(i).isDigit) {
                val is_double = {
                    var j = i + 1
                    while (j < p.length && p(j).isDigit) {
                        j += 1
                    }
                    j < p.length && (p(j) == '.' || p(j) == 'e' || p(j) == 'E')
                }
                val minus_flag = if (i > 0 && p(i - 1) == '-') {
                    -1
                } else {
                    1
                }
                var j = i
                while (j < p.length && p(j) != ',' && p(j) != ']' && p(j) != '}') {
                    j += 1
                }
                if (is_double) {
                    val v = p.substring(i, j).toDouble * minus_flag
                    sta.append(('d', v))
                } else {
                    val v = p.substring(i, j).toLong * minus_flag
                    sta.append(('i', v))
                }
                i = j - 1
            } else if (p.substring(i, i + 4) == "null") {
                sta.append(('n', null))
                i += 3
            } else if (p.substring(i, i + 4) == "true") {
                sta.append(('b', true))
                i += 3
            } else if (p.substring(i, i + 5) == "false") {
                sta.append(('b', false))
                i += 4
            } else {
                throw new Exception("parse error:unrecognized character##" + p.substring(i) + "##pos:" + i)
            }
            i += 1
        }
        if (sta.length != 1) {
            throw new Exception("parse error, type=final")
        }
        Value(sta.head._2)
    }
}
