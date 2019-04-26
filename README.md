# monitor
Monitor is a multi-threaded server monitor with built-in notification system.

<b>How to compile to standalone binary:</b>
<pre>
raco exe monitor.rkt
raco distribute release monitor
cp monitor.conf servers.conf monitor.html ./release/bin
</pre>

<b>How to use:</b>
<pre>
racket monitor.rkt path/to/monitor-config-file.conf /path/to/servers-config-file.conf /path/to/html-email-template.html
</pre>
or more simply, once compiled, from the current directory:
<pre>
./monitor monitor.conf servers.conf monitor.html
</pre>

<b>Sample output:</b>
<pre>
Starting scan...

deswww01
Uptime: 13:05:06 up 134 days
CPU Usage: 0.31, 0.28, 0.22
RAM Usage: 75%
DSK Usage: 86%
Top 3 CPU: mysqld 10.3 sshd 2.0 sshd 2.0
Top 3 RAM: mysqld 63.4 php-fpm7.0 1.2 php-fpm7.0 1.1

deswww02
Uptime: 13:06:37 up 17 days
CPU Usage: 0.22, 0.43, 0.34
RAM Usage: 37%
DSK Usage: 14%
Top 3 CPU: mysqld 15.1 php-fpm7.0 1.1 php-fpm7.0 0.9
Top 3 RAM: mysqld 26.7 php-fpm7.0 2.1 php-fpm7.0 1.4

desvps01
Uptime: 13:06:37 up 22 days
CPU Usage: 0.00, 0.01, 0.05
RAM Usage: 96%
DSK Usage: 8%
Top 3 CPU: mysqld 2.1 sshd 1.0 sshd 1.0
Top 3 RAM: mysqld 3.6 telegraf 0.3 apache2 0.2

All done.
</pre>
