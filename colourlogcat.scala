#!/usr/bin/env scala

import scala.Console._
import scala.util.matching.Regex
import scala.collection.mutable

val MaxNameLength = 18
val CycleColours = Array(
		s"$BLUE$GREEN_B",
		s"$CYAN$GREEN_B",
		s"$BLACK$GREEN_B", 
		s"$MAGENTA$GREEN_B", 
		s"$WHITE$GREEN_B", 
		s"$YELLOW$GREEN_B",
		s"$BOLD$BLUE$GREEN_B",
		s"$BOLD$CYAN$GREEN_B",
		s"$BOLD$BLACK$GREEN_B", 
		s"$BOLD$MAGENTA$GREEN_B", 
		s"$BOLD$WHITE$GREEN_B", 
		s"$BOLD$YELLOW$GREEN_B"
	)
val PresetNameColours = Map(
		"System.err" -> s"$BLACK$RED_B",
		"dalvikvm" -> s"$RESET$MAGENTA",
		"dalvikvm-heap" -> s"$RESET$MAGENTA",
		"ActivityManager" -> s"$RESET$GREEN",
		"PackageManager" -> s"$RESET$CYAN",
		"libEGL" -> s"$RESET$BLUE",
		"wpa_supplicant" -> s"$RESET$YELLOW",
		"InputMethodManager" -> s"$RESET$BLUE",
		"System.out" -> s"$RESET$YELLOW"
	)
val TagColours = Map(
		"V" -> s"$RESET$BOLD$WHITE$BLUE_B",
		"D" -> s"$RESET$BOLD$YELLOW$BLUE_B",
		"I" -> s"$RESET$BOLD$CYAN$BLUE_B",
		"W" -> s"$RESET$BLACK$YELLOW_B",
		"E" -> s"$RESET$BLACK$RED_B",
		"F" -> s"$RESET$BLACK$MAGENTA_B"
	)
val DefaultTagColour = s"$RESET$BLUE_B"

val parse = """(\w)\/([^\(]+)\(\s*(\d+)\)\:(.*)""".r
val parseTime = """\d\d\d\d\d\d\.\d\d\d\d\d\d\.\d\d\d""".r
val parseBlock = """\[[\s\w\d_\.]+\]""".r
val parseHttp = """GET|PUT|POST|DELETE""".r
val parseUrl = """(https?:\/\/)([\da-z\.-]+)\.([a-z\.]{2,6})\:?([\/\w \.-\?=]*)*\/?""".r

val nameColours = mutable.Map.empty[Int, String]
var nameColourCycle = 0

def getNameColour(name: String, proc: Int): String = {
	if (PresetNameColours.contains(name)) {
		PresetNameColours(name)
	}
	else {
		if (!nameColours.contains(proc)) {
			nameColours(proc) = CycleColours(nameColourCycle)
			nameColourCycle += 1
			if (nameColourCycle == CycleColours.length) nameColourCycle = 0
		}
		nameColours(proc)
	}
}

def paintMessage(message: String): String = {
	val timeStamps = parseTime replaceAllIn (message, s"$YELLOW$$0$RESET")
	val blocks = parseBlock replaceAllIn (timeStamps, s"$BOLD$GREEN$BLUE_B$$0$RESET")
	val http = parseHttp replaceAllIn (blocks, s"$BOLD$YELLOW$$0$RESET")
	val url = parseUrl replaceAllIn (http, s"$BOLD$BLUE$$0$RESET")
	url
}

var line = readLine()
while (line != null) {
	//println(line)
	for (parse(rawTag, rawName, rawProc, rawMessage) <- parse.findAllIn(line)) {
		val tag = rawTag.trim()
		val name = rawName.trim().take(MaxNameLength)
		val proc = rawProc.trim().toInt
		val nameColour = getNameColour(name, proc)
		val message = paintMessage(rawMessage.trim())
		val tagColour = TagColours.get(tag).getOrElse(DefaultTagColour)
		println(f"$nameColour%s$name%18s $tagColour%s $tag%1s $RESET%s $message%s")
	}
	line = readLine()
}
