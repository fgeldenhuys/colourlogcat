#!/usr/bin/env ruby
require 'colorize'

$available_colours = [:red, :green, :yellow, :blue, :magenta, :cyan, :light_red, :light_green, :light_yellow, :light_blue, :light_magenta, :light_cyan]
$tag_colours = {}
$next_colour = 1
$max_tag_length = 10

level = ''
termw = `tput cols`.to_i

last_time = Time.now

ARGF.each_line {|line|
	now = Time.now
	timedif = now - last_time
	if timedif > 5
		puts ('+%.2f' % timedif.to_s + ' seconds    ' + now.strftime('%H:%M:%S')).center(termw).cyan.on_blue
	end

	if m = line.match(/(\w)\/(\w+)[^:]*:\s*(.*)/)
		
		level = m[1].to_s.center(3)
		if m[1] == 'V' then
			level = level.white.on_blue
		elsif m[1] == 'D' then
			level = level.magenta.on_blue
		elsif m[1] == 'I' then
			level = level.cyan.on_blue
		elsif m[1] == 'W' then
			level = level.black.on_yellow
		elsif m[1] == 'E' then
			level = level.black.on_light_red
		elsif m[1] == 'F' then
			level = level.black.on_light_magenta
		end
		
		tag = m[2]
		colour = $tag_colours[tag]
		if not colour then
			colour = $tag_colours[tag] = $available_colours[$next_colour]
			$next_colour += 1
			$next_colour = 1 if $next_colour >= $available_colours.size
		end
		$max_tag_length = tag.size + 2 if tag.size > $max_tag_length
		tag = tag.rjust($max_tag_length)
		tag = tag.colorize(:color => colour, :background => :default)

		message = m[3]
		pad = $max_tag_length + 5
		space = termw - pad - 1

		puts(tag + ' ' + level + ' ' + message[0..space])
		while message do
			message = message[(space+1)..-1]
			puts((' ' * $max_tag_length) + ' ' + level + ' ' + message[0..space]) if message
		end

	else

		message = line
		pad = $max_tag_length + 5
		space = termw - pad - 1

		while message do
			puts((' ' * $max_tag_length) + ' ' + level + ' ' + message[0..space]) if message
			message = message[(space+1)..-1]
		end

	end
	last_time = Time.now
}
