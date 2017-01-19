var data = [4, 8, 15, 16, 23, 42];

var chart = d3.select("body").append("div").attr("class", "chart");

//chart.selectAll("div")
//        .data(data)
//    .enter().append("div")
//        .style("width", function(d) { return d * 10 + "px"; })
//        .text(function(d) { return d; });

var x = d3.scale.linear().domain([0, d3.max(data)]).range(["0px", "420px"]);
chart.selectAll("div")
        .data(data)
    .enter().append("div")
        .style("width", x)
        .text(String);

