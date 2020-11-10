

println("Welcome to the Scala worksheet")

val f: String => String = {case "ping" => "pong"}

f("ping")
// f("abc") // MatchError

val p: PartialFunction[String, String] = {case "ping" => "pong"}
p("ping")
p.isDefinedAt("abc")
p.isDefinedAt("ping")



