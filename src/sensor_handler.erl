-module(sensor_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        {QsVal, Req2} = cowboy_req:qs_val(<<"id">>, Req),
	{ok, Req3} = cowboy_req:reply(200,
            local_http_utils:headers(),
            [
                 "<html><head>",
                 include(),
                 raw_data(QsVal),
                 script(),
                 "</head><body>",
                 "<h1>Sensor: ", QsVal, "</h1>",
                 "<div id='temp_chart' style='width: 900px; height: 500px'></div>",
                 "<div id='hum_chart' style='width: 900px; height: 500px'></div>",
                 "</body></html>"
            ],
            Req2),
	{ok, Req3, State}.

include() ->
   "<script type='text/javascript' src='https://www.gstatic.com/charts/loader.js'></script>".

raw_data(QueryId) ->
        Id = id_to_integer(QueryId),
        TempRawData = data_store:get_data({Id,t}),
        Temps = [io_lib:format("[~p,~p]", [T, D]) || {_, _, D ,T} <- TempRawData],
        Temp = string:join(Temps, ","),

        HumRawData = data_store:get_data({Id,h}),
        Hums = [io_lib:format("[~p,~p]", [T, D]) || {_, _, D ,T} <- HumRawData],
        Hum = string:join(Hums, ","),

        "<script type=\"text/javascript\">\nvar temp_data_t = [\n" ++
            Temp ++ "\n];\nvar hum_data_t = [" ++ Hum ++ "\n];\n</script>".

id_to_integer(Bin) when is_binary(Bin) -> binary_to_integer(Bin);
id_to_integer([_] = Id) -> list_to_integer(Id).

script() ->
"<script type='text/javascript'>
      google.charts.load('current', {'packages':['corechart'], 'language':'sv'});
      google.charts.setOnLoadCallback(drawChart);

      function drawChart() {
        var temp_data = new google.visualization.DataTable();
        temp_data.addColumn('number', 'Date');
        temp_data.addColumn('number', 'Temperature');
        temp_data.addRows(temp_data_t);

        var options = {
          title: 'Temperature',
          legend: { position: 'bottom' },
          colors: ['red'],
          vAxis: { minValue: 10 }
        };

        var temp_chart = new google.visualization.LineChart(document.getElementById('temp_chart'));

        var temp_view = new google.visualization.DataView(temp_data);
        temp_view.setColumns([{
               type: 'datetime',
               label: temp_data.getColumnLabel(0),
               calc: function(dt, row) {
                         return new Date(dt.getValue(row, 0) * 1000);
                         }
        }, 1]);

        temp_chart.draw(temp_view, options);

        // Next chart

        var hum_data = new google.visualization.DataTable();
        hum_data.addColumn('number', 'Date');
        hum_data.addColumn('number', 'Humidity');
        hum_data.addRows(hum_data_t);

        var options = {
          title: 'Humidity',
          legend: { position: 'bottom' },
          vAxis: {
              viewWindowMode:'explicit',
              viewWindow:{
                max:105,
                min:0
              }
            }
        };

        var hum_chart = new google.visualization.LineChart(document.getElementById('hum_chart'));

        var hum_view = new google.visualization.DataView(hum_data);
        hum_view.setColumns([{
               type: 'datetime',
               label: hum_data.getColumnLabel(0),
               calc: function(dt, row) {
                         return new Date(dt.getValue(row, 0) * 1000);
                         }
        }, 1]);

        hum_chart.draw(hum_view, options);
      }
    </script>".

terminate(_Reason, _Req, _State) ->
	ok.
