var n = 40;

function chart(interpolation, allData) {
    var margin = {top: 6, right: 0, bottom: 20, left: 40},
        width = 960 - margin.right,
        height = 320 - margin.top - margin.bottom;

    var x = d3.time.scale()
        .domain([firstOf(firstOf(allData)).date, lastOf(firstOf(allData)).date])
        .range([0, width]);
    var y = d3.scale.linear()
        .domain([minValueIn(allData), maxValueIn(allData)])
        .range([height, 0]);

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

    for (i = 0; i < allData.length; i++) {
        var line = d3.svg.line()
            .interpolate(interpolation)
            .x(function(d) { return x(d.date); })
            .y(function(d) { return y(d.value); });

        svg.append("g")
            .attr("clip-path", "url(#clip)")
            .append("path")
            .data([allData[i]])
            .attr("class", "line" + i)
            .attr("d", line);
    }

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

function firstOf(data) { return data[0]; }
function lastOf(data) { return data[data.length - 1]; }

function adapt(data) {
    var parse = d3.time.format("%d/%m/%Y").parse;
    var adaptedData = data.v;
    adaptedData.forEach(function (d) { d.date = parse(d.date); });
    return adaptedData;
}

$.getJSON("/quote/YHOO", function (myData1) {
    chart("linear", [adapt(myData1)]);
});

$.getJSON("/buy/YHOO", function (myData2) {
    $.getJSON("/sell/YHOO", function (myData3) {
        chart("linear", [adapt(myData2), adapt(myData3)]);
    });
});

$.getJSON("/macd/YHOO", function (myData2) {
    $.getJSON("/macdSignal/YHOO", function (myData3) {
        chart("linear", [adapt(myData2), adapt(myData3)]);
    });
});
