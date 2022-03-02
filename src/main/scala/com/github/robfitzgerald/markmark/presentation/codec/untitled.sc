import scala.util.matching.Regex

val EnvRegex: Regex = "(?s)\\\\begin\\{(\\w+)} *(\\{\\w+})? *(.+?)\\\\end\\{(\\w+)}".r

val s = """\begin{asasdf253}{yip}
            |	x foof
            |\end{asad1234}
            |""".stripMargin.trim

s match {
  case EnvRegex(start, title, env, end) =>
    println(start)
    println(title)
    println(env.trim)
    println(end)
  case other =>
    println("ugh")
}