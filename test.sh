ping -i 0.3 192.168.1.1 | sed -u 's/.*time=\(.*\)ms/\1/g' | grep -v --line-buffered PING
