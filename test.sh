if [ "$#" -gt 0 ] ; then
    dom="$1"
else
    dom="192.168.1.1"
fi

ping -i 0.3 "$dom" | sed -u 's/.*time=\(.*\)ms/\1/g' | grep -v --line-buffered PING
