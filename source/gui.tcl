#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

### Starts up prolog and establishes connections 

proc pl_open {fname tw} { 
    set p [open "\|xpce -f $fname -g true -t 'gui'" r+] 
    gets $p answer 
    while {$answer != "INIT"} { 
	gets $p answer 
    } 
    $tw configure -state normal 
    $tw insert end "START\n" 
    $tw configure -state disabled 
    return $p 
}

proc pl_load {p tw file} {

    puts $p "load($file).\n"
    flush $p

    gets $p answer
    while {$answer != "INIT"} {
        gets $p answer
    }

}

proc pl_parse {p tw sent} {

    puts $p "parse($sent).\n"
    flush $p
    
    $tw configure -state normal 
    gets $p answer
    $tw insert end $answer
    while {![string is upper -strict $answer]} {
	gets $p answer
	$tw insert end $answer
    }

    if {$answer != "CHOOSE ACTIVE"} {
	return
    }

    # read list
    set i 0
    $tw configure -state normal 
    gets $p answer
    while {![string is upper -strict $answer]} {
	gets $p answer
	$tw insert end "$i. $answer\n" 
    }
    $tw configure -state disabled 
    
}

### Closes all the connections and quits 

proc pl_close {p tw} { 
    $tw insert end "?- quit.\n" 
    close $p 
    exit 
}

proc pl_test {p tw} {

    puts $p "test.\n"
    flush $p
    $tw configure -state normal
    gets $p answer
    while {$answer != "INIT"} {
	$tw insert end "$answer"
	gets $p $answer
    }
    $tw configure -state disabled
}

##### Define prolog interaction 
### Sends query to prolog and collects all answers 

proc pl_command {p query tw} { 
    # Display the query 
    $tw configure -state normal 
    $tw insert end "$query\.\n" 
    $tw configure -state disabled 
    # Send the query to prolog 
    puts $p "$query.\n" 
    flush $p 
    # Get the first answer if any 
    gets $p answer 
    while {$answer != "INIT"} { 
	# Print the answer 
	$tw configure -state normal 
	$tw insert end "   $answer\n" 
	$tw configure -state disabled 
	update idletasks
	# Collect the next answer 
	gets $p answer 
    } 
    $tw configure -state normal 
    $tw insert end "\n?- " 
    $tw configure -state disabled 
}

##### Define query widgets 

frame .query -relief raised -bd 2 
label .query.label1 -text "Query:" 
entry .query.text -textvariable pro_query 
label .query.label2 -text "Result?" 
entry .query.result -width 10 -textvariable pro_result 
button .query.send -text "Send" -bd 2 \
   -command { pl_command $plfile $pro_query .answer.text } 
#button .query.send -text "Send" -bd 2 \
#   -command { pl_command $plfile $pro_query $pro_result .answer.text } 
button .query.quit -text "Quit" -bd 2 \
   -command { pl_close $plfile .answer.text } 

##### Define answer widgets 

frame .answer -relief raised -bd 2 
label .answer.label -text "Log:" 
text .answer.text \
   -font -adobe-courier-bold-r-normal-*-12-*-*-*-*-*-*-* \
   -wrap word \
   -relief raised -bd 2 \
   -state disabled \
   -yscrollcommand ".answer.scroll set" 
scrollbar .answer.scroll -command ".answer.text yview" 

##### Show all widgets 

pack .query -side top -fill x 
pack .query.label1 -side left 
pack .query.text -side left -fill x -expand 1 
pack .query.label2 -side left 
pack .query.result -side left 
pack .query.quit -side right 
pack .query.send -side right 
pack .answer -side top -fill both -expand 1 
pack .answer.label -side top 
pack .answer.text -side left -fill both -expand 1 
pack .answer.scroll -side right -fill y 

##### Setup communication with Prolog 

set pro_query "load(const_test)"
set pro_result ""
#set plfile [pl_open "id_pro.pl" .answer.text] 
set plfile [pl_open "chart_var.pl" .answer.text] 
pl_load  $plfile .answer.text "const_test.pl"
#pl_test  $plfile .answer.text
#pl_parse $plfile .answer.text 133