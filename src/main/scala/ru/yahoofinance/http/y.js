var n = 40;

function chart(interpolation, myData, myData2) {
    var margin = {top: 6, right: 0, bottom: 20, left: 40},
        width = 960 - margin.right,
        height = 320 - margin.top - margin.bottom;

    var x = d3.time.scale()
        .domain([myData[0].date, myData[myData.length - 1].date])
        .range([0, width]);
    var y = d3.scale.linear()
        .domain([0, d3.max(myData, function (d){ return d.value; })])
        .range([height, 0]);
    var line = d3.svg.line()
        .interpolate(interpolation)
        .x(function(d) { return x(d.date); })
        .y(function(d) { return y(d.value); });

    var y2 = d3.scale.linear()
        .domain([0, d3.max(myData2, function (d){ return d.value; })])
        .range([height, 0]);
    var line2 = d3.svg.line()
        .interpolate(interpolation)
        .x(function(d) { return x(d.date); })
        .y(function(d) { return y2(d.value); });

    var svg = d3.select("body").append("p").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .style("margin-left", margin.left + "px")
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    svg.append("defs").append("clipPath")
        .attr("id", "clip")
        .append("rect")
        .attr("width", width)
        .attr("height", height);

    svg.append("g")
        .attr("clip-path", "url(#clip)")
        .append("path")
        .data([myData])
        .attr("class", "line")
        .attr("d", line);
    svg.append("g")
        .attr("clip-path", "url(#clip)")
        .append("path")
        .data([myData2])
        .attr("class", "line2")
        .attr("d", line2);

    svg.append("g")
        .attr("class", "y axis")
        .call(d3.svg.axis().scale(y).ticks(10).orient("left"));

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + (height) + ")")
        .call(d3.svg.axis().scale(x).tickSize(-height).tickSubdivide(false).tickFormat(d3.time.format("%d/%m/%y")).ticks(10).orient("bottom"));
}

$.getJSON("/quote/YHOO", function (myData1) {
    $.getJSON("/ema/YHOO", function (myData2) {
        var parse = d3.time.format("%d/%m/%Y").parse;
        var adaptedData = myData1.v;
        adaptedData.forEach(function (d) { d.date = parse(d.date); });

        var adaptedData2 = myData2.v;
        adaptedData2.forEach(function (d) { d.date = parse(d.date); });
//        adaptedData2.forEach(function (d) { d.value = d.value; }); // TODO add separate scale?

        chart("linear", adaptedData, adaptedData2);
    });
});
