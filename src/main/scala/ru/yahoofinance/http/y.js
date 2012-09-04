var n = 40;

function chart(interpolation, myData, myData2) {
    var margin = {top: 6, right: 0, bottom: 20, left: 40},
        width = 960 - margin.right,
        height = 320 - margin.top - margin.bottom;

    var x = d3.time.scale()
        .domain([myData[0].date, myData[myData.length - 1].date])
        .range([0, width]);
    var y = d3.scale.linear()
        .domain([minValueIn([myData, myData2]), maxValueIn([myData, myData2])])
        .range([height, 0]);
    var line = d3.svg.line()
        .interpolate(interpolation)
        .x(function(d) { return x(d.date); })
        .y(function(d) { return y(d.value); });

    var line2 = d3.svg.line()
        .interpolate(interpolation)
        .x(function(d) { return x(d.date); })
        .y(function(d) { return y(d.value); });

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

function find(data, func, compare) {
    if (data instanceof Array && data.length > 0 && (data[0] instanceof Array)) {
        var pivot = Number.NaN;
        for (i = 0; i < data.length; i++) {
            var value = func(data[i]);
            if (isNaN(pivot) || compare(value, pivot)) {
                pivot = value;
            }
        }
        return pivot;
    } else {
        return func(data);
    }
}

function maxValueIn(data) {
    return find(
        data,
        function(d){ return d3.max(d, function(d2){ return d2.value; }); },
        function(value, pivot) { return value > pivot; }
    );
}

function minValueIn(data) {
    return find(
        data,
        function(d){ return d3.min(d, function(d2){ return d2.value; }); },
        function(value, pivot){ return value < pivot; }
    );
}

function adapt(data) {
    var parse = d3.time.format("%d/%m/%Y").parse;
    var adaptedData = data.v;
    adaptedData.forEach(function (d) { d.date = parse(d.date); });
    return adaptedData;
}

$.getJSON("/quote/YHOO", function (myData1) {
    $.getJSON("/macd/YHOO", function (myData2) {
//        adaptedData2.forEach(function (d) { d.value = d.value; }); // TODO add separate scale?
        chart("linear", adapt(myData1), adapt(myData2));
    });
});
