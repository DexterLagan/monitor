# Monitor
Monitor is a multi-threaded server monitor with built-in notification system.

## Concept
Monitor runs any number of commands on remote servers through SSH tunnels and displays results on the screen. If CPU usage on any of the specified systems goes above 1, or if RAM or disk usage goes to 100%, Monitor emails the specified distribution list an alert with the system statistics. All servers are contacted in parallel and all commands are run in parallel on each server. Notification e-mails are also sent in parallel. Monitor supports an unlimited number of servers and commands and is easily extensible through simple configuration files. To run Monitor continuously at fixed interval, add a call with full paths to your crontab.

## Installation
<pre>
git clone https://github.com/DexterLagan/monitor
</pre>

## Build Instructions
<pre>
raco exe monitor.rkt
raco distribute release monitor
cp monitor.conf servers.conf monitor.html ./release/bin
</pre>

## Usage
<pre>
racket monitor.rkt path/to/monitor-config-file.conf /path/to/servers-config-file.conf /path/to/html-email-template.html
</pre>
or more simply, once compiled, from the current directory:
<pre>
./monitor monitor.conf servers.conf monitor.html
</pre>
Note that all target servers should have their **authorized-keys** file up to date with the monitoring system's public key.

<b>How to extend Monitor:</b><br><br>
To increase the number of commands to run, first increase the default command count in monitor.rkt through the <pre>*app-config-section-length*</pre> constant, then add a <pre>(get-config-item-or-die app-config "my-new-command-title=")</pre> line to the commands list definition in the Main section.

<b>Sample monitor configuration file:</b>
<pre>
[Monitor]
distribution-list=*******@gmail.com,*******@gmail.com
hostname-command=hostname
uptime-command=echo "Uptime:"`uptime | cut -d "," -f 1`
cpu-usage-command=echo "CPU Usage:"`uptime | cut -d ":" -f 5`
ram-usage-command=free | grep -i mem | awk "{printf \"RAM Usage: %i%%\",\$3/\$2 * 100.0}"
dsk-usage-command=echo "DSK Usage: "`df -H | grep /dev/sda | sed "s/.*[ \t][ \t]*\([0-9][0-9]*\)%.*/\1%/"`
top-cpu-command=echo "Top 3 CPU: "`ps --no-headers -eo comm,pcpu --sort=-%cpu | head -n 3`
top-ram-command=echo "Top 3 RAM: "`ps --no-headers -eo comm,pmem --sort=-%mem | head -n 3`
</pre>

<b>Sample servers configuration file:</b>
<pre>
[server #1]
servername=My Server #1
hostname=some-server1.some-domain.com
username=some-user

[server #2]
servername=My Server #2
hostname=some-server2.some-domain.com
username=some-user

[server #3]
servername=My Server #3
hostname=some-server3.some-domain.com
username=some-user
</pre>

<b>Sample output:</b>
<pre>
Starting scan...

some-server-1
Uptime: 13:05:06 up 134 days
CPU Usage: 0.31, 0.28, 0.22
RAM Usage: 75%
DSK Usage: 86%
Top 3 CPU: mysqld 10.3 sshd 2.0 sshd 2.0
Top 3 RAM: mysqld 63.4 php-fpm7.0 1.2 php-fpm7.0 1.1

some-server-2
Uptime: 13:06:37 up 17 days
CPU Usage: 0.22, 0.43, 0.34
RAM Usage: 37%
DSK Usage: 14%
Top 3 CPU: mysqld 15.1 php-fpm7.0 1.1 php-fpm7.0 0.9
Top 3 RAM: mysqld 26.7 php-fpm7.0 2.1 php-fpm7.0 1.4

some-server-3
Uptime: 13:06:37 up 22 days
CPU Usage: 0.00, 0.01, 0.05
RAM Usage: 96%
DSK Usage: 8%
Top 3 CPU: mysqld 2.1 sshd 1.0 sshd 1.0
Top 3 RAM: mysqld 3.6 telegraf 0.3 apache2 0.2

All done.
</pre>

## License

Monitor is free software; see [LICENSE](https://github.com/DexterLagan/monitor/blob/main/LICENSE) for more details.
