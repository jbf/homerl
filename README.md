# homerl

Homerl is a home monitoring system written in erlang working with the [Telldus TellStick Duo][1] transmitter/receiver. Homerl collects data from your attached sensors; currently temperature and humidity are supported. Sensor data are displayed using [Google Charts][2] from an embedded [Cowboy][3] web server.

![Screenshot](/../screenshot/index.png?raw=true "Homerl sensor data")

[1]: http://www.telldus.se/products/tellstick_duo "TellStick Duo"
[2]: https://developers.google.com/chart/ "Google Charts"
[3]: http://ninenines.eu/ "Cowboy"
