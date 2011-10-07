##############################
## functions
##############################
# showa: remind myself about aliases
showa () { /usr/bin/grep -i -a1 $@ ~/.aliases.bash | grep -v '^\s*$' ; }

# sourcea: refresh aliases file
alias sourcea='source ~/.aliases.bash'

# edital: edit aliases file in textmate
alias edital='mate ~/.aliases.bash'

# rm_DS_Store_files: removes all .DS_Store file from the current dir and below
alias rm_DS_Store_files='find . -name .DS_Store -exec rm {} \;'

# zipf: to create a ZIP archive of a folder
zipf () { zip -r "$1".zip "$1" ; }

##############################
##### SEARCHING
##############################

# ff:  to find a file under the current directory
ff () { /usr/bin/find . -name "$@" ; }

# ffs: to find a file whose name starts with a given string
ffs () { /usr/bin/find . -name "$@"'*' ; }

# ffe: to find a file whose name ends with a given string
ffe () { /usr/bin/find . -name '*'"$@" ; }

# locatemd: to search for a file using Spotlight's metadata
function locatemd {  mdfind "kMDItemDisplayName == '$@'wc"; }

# locaterecent: to search for files created since yesterday using Spotlight
# This is an illustration of using $time in a query
# See: http://developer.apple.com/documentation/Carbon/Conceptual/SpotlightQuery/index.html
function locaterecent { mdfind 'kMDItemFSCreationDate >= $time.yesterday'; }


##############################
##### PROCESSES
##############################

alias pstree='/sw/bin/pstree -g 2 -w'

# to find memory hogs:
alias mem_hogs_top='top -l 1 -o rsize | head -20'
alias mem_hogs_ps='ps wwaxm -o pid,stat,vsize,rss,time,command | head -10'

# to find CPU hogs
alias cpu_hogs='ps wwaxr -o pid,stat,%cpu,time,command | head -10'

# continual 'top' listing (every 10 seconds)
alias topforever='top -l 9999999 -s 10 -o cpu'


##############################
##### NETWORKING
##############################

# ip_info: to get info on DHCP server, router, DNS server, etc (for en0 or en1)
alias ip_info='ipconfig getpacket en1'

# browse_bonjour: browse services advertised via Bonjour
# Note: need to supply a "type" argument- e.g. "_http._tcp"
# See http://www.dns-sd.org/ServiceTypes.html for more types
# Optionally supply a "domain" argument
alias browse_bonjour='dns-sd -B'

# debug_http: download a web page and show info on what took time
debug_http () { /usr/bin/curl $@ -o /dev/null -w "dns: %{time_namelookup} connect: %{time_connect} pretransfer: %{time_pretransfer} starttransfer: %{time_starttransfer} total: %{time_total}\n" ; }


##############################
##### SYSTEM & OS
##############################

# install all software updates from the command line
alias software_update_cmd='COMMAND_LINE_INSTALL=1 export COMMAND_LINE_INSTALL; sudo softwareupdate -i -a'

# finderTurnOffDesktop: turn off display of files on the Desktop
alias finderTurnOffDesktop='defaults write com.apple.finder CreateDesktop FALSE'

# finderTurnOnDesktop: turn off display of files on the Desktop
alias finderTurnOnDesktop='defaults write com.apple.finder CreateDesktop TRUE'

function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "("${ref#refs/heads/}")"
}

RED="\[\033[0;31m\]"
YELLOW="\[\033[1;33m\]"
GREEN="\[\033[1;32m\]"
WHITE="\[\033[1;37m\]"

PS1="$WHITE\$(date +%H:%M) \w$YELLOW \$(parse_git_branch)$WHITE\$ "

##############################
##### SCM (git/svn)
##############################
alias gch='git checkout'                          #check out repo
alias gco='git checkout'                          #check out repo
alias gst='git status'                            #current status of the git repo
alias gist='git status'                           #current status of the git repo
alias gl='git pull'
alias gp='git push'
alias gd='git diff | mate'
alias gcommit='git commit -v'                     #simple commit including diff in message
alias gcm='git commit -v -m'                      #commit with custom message and diff in message
alias gca='git commit -v -a -m'                   #all of the above commits but also opens to view
alias gb='git branch --color'                     #create branch                     
alias gba='git branch -a --color'
alias gi='mate .gitignore'                        #edit list of global files to ignore
alias gls='git ls-files'                          #current files being managed
alias gall='git add .'                            #add modified files to be committed
alias glo='git log'                               #displays log for git repo
alias glos='git log --stat'                       #displays log with added stats
alias glop='git log -p'                           #displays log with differences

alias gpodevel='git push origin development:development'
alias gpowork='git push origin working:working'
alias gpostage='git push origin staging:staging'
alias gpoprod='git push origin master:master'


##############################
## FILE SYSTEM
##############################

alias tree='find . -print | sed -e "s;[^/]*/;|____;g;s;____|; |;g"'
alias l='ls -a'
alias ll='ls -la'

#### Shortcuts
alias work='cd ~/Documents/work | ls'
alias andculture='cd ~/Documents/work/andculture | ls'
alias adeptiv='cd ~/Documents/work/andculture/ad*land/source/ad*'
alias hydra='cd ~/Documents/work/andculture/Hydra/'
alias bat='cd ~/Documents/work/andCulture/BaitulNikah_integration/source/b*'
alias config='cd ~/Documents/personal/ConfigurationFiles'
alias cpa='cd ~/Documents/work/andCulture/AbsorptionCellportAnalytics/absorption_cpa'
alias mnmc='cd ~/Documents/work/andCulture/Mount\ Nittany/MNMC/'
alias pa='cd ~/Documents/work/andCulture/PADowntown/padowtown_web/'
alias ruth='cd ~/Documents/work/andCulture/HouseOfRuth/House-of-Ruth/'
alias hooverMax='cd ~/Documents/work/andCulture/Hoover/Hoover-MaxedOut/'
alias getsat='cd ~/Documents/work/andCulture/GetSatisfaction/Widget/repo/getsat.widget'
##
# Your previous /Users/wintondeshong/.bash_profile file was backed up as /Users/wintondeshong/.bash_profile.macports-saved_2011-06-13_at_12:27:42
##

# MacPorts Installer addition on 2011-06-13_at_12:27:42: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

