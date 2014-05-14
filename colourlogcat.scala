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
		s"$BOLD$CYAN$GREEN_B",
		s"$BOLD$BLACK$GREEN_B", 
		s"$BOLD$MAGENTA$GREEN_B", 
		s"$BOLD$WHITE$GREEN_B", 
		s"$BOLD$YELLOW$GREEN_B"
	)
val PresetNameColours = Map(
		"System.out" -> s"$RESET$YELLOW",
		"System.err" -> s"$BLACK$RED_B",
		"libEGL" -> s"$RESET$BLUE",
		"PowerUI" -> s"$RESET$BLUE",
		"wpa_supplicant" -> s"$RESET$YELLOW",
		"TabletStatusBar" -> s"$RESET$GREEN",
		"Settings" -> s"$RESET$CYAN",
		"BroadcastQueue" -> s"$RESET$CYAN"
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
val parseBlock = """\[[\s\w\d_\.\:<>\$]+\]""".r
val parseEmphasis = """\s[A-Z_\-]+[A-Z_\-0-9]*\s""".r
val parseUrl = """(https?:\/\/)([\da-z\.-]+)\.([a-z\.]{2,6})\:?([\/\w \.-\?=\-\&%]*)*\/?""".r
val parseJsLog = """(\w+:\/\/\/[\da-z\.-]+)(:\d+:)""".r

val nameColours = mutable.Map.empty[Int, String]
var nameColourCycle = 0

var nameLength = 5
var reduceCounter = 0

def cols = (scala.sys.process.Process("tput cols")!!).trim().toInt

def space(n: Int) = Seq.fill[String](n)(" ").mkString

def getNameColour(name: String, proc: Int): String = {
	if (PresetNameColours.contains(name)) {
		PresetNameColours(name)
	}
	else if (name.startsWith("dalvik")) s"$RESET$MAGENTA"
	else if (name.endsWith("Service")) s"$RESET$CYAN"
	else if (name.endsWith("Manager")) s"$RESET$BLUE"
	else if (name.endsWith("Monitor")) s"$RESET$YELLOW"
	else if (name.endsWith("Scheduler")) s"$RESET$BOLD$BLUE"
	else if (name.endsWith("Thread")) s"$RESET$BOLD$MAGENTA"
	else {
		if (!nameColours.contains(proc)) {
			nameColours(proc) = CycleColours(nameColourCycle)
			nameColourCycle += 1
			if (nameColourCycle == CycleColours.length) nameColourCycle = 0
		}
		nameColours(proc)
	}
}

def trimName(name: String): String = {
	if (name.length > nameLength) {
		nameLength += 1
		reduceCounter = 0
	}
	else if (name.length < nameLength) {
		reduceCounter += 1
		if (reduceCounter == 30) {
			nameLength -= 1
			reduceCounter = 0
		}
	}
	else {
		reduceCounter = 0
	}
	if (nameLength > MaxNameLength) nameLength = MaxNameLength
	val length = math.min(name.length, nameLength)
	(" " * (nameLength - length)) + (name take nameLength)
}

def paintMessage(message: String): String = {
	val timeStamps = parseTime replaceAllIn (message, s"$YELLOW$$0$RESET")
	val blocks = parseBlock replaceAllIn (timeStamps, s"$BOLD$CYAN$$0$RESET")
	val emphasis = parseEmphasis replaceAllIn (blocks, s"$BOLD$YELLOW$$0$RESET")
	val url = parseUrl replaceAllIn (emphasis, s"$BOLD$BLUE$$0$RESET")
	val jslog = parseJsLog replaceAllIn (url, s"$BOLD$MAGENTA$$1$RESET$YELLOW$$2$RESET")
	jslog
}

var mark = System.currentTimeMillis
var line = readLine()
while (line != null) {
	val dt = System.currentTimeMillis - mark
	if (dt > 2000) {
		val c = cols
		val show = s"$dt"
		val s = (c - show.length) / 2
		println(s"$BOLD$CYAN$BLUE_B" + space(s) + show + space(s) + RESET)
	}
	for (parse(rawTag, rawName, rawProc, rawMessage) <- parse.findAllIn(line)) {
		val tag = rawTag.trim()
		val name = trimName(rawName.trim())
		val proc = rawProc.trim().toInt
		val nameColour = getNameColour(rawName.trim(), proc)
		val message = paintMessage(rawMessage.trim())
		val tagColour = TagColours.get(tag).getOrElse(DefaultTagColour)
		if (rawName.trim().startsWith("dalvik") && rawMessage.indexOf("with exception pending") != -1) {
			val c = cols
			val show = "EXCEPTION PENDING!"
			val s = (c - show.size) / 2
			println(s"$BLACK$RED_B" + space(s) + show + space(s) + RESET)
		}
		println(s"$nameColour$name $tagColour $tag $RESET $message")
	}
	mark = System.currentTimeMillis
	line = readLine()
}
