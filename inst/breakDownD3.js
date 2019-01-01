// !preview r2d3 data=list(structure(list("* Sex = female", 0.306481792717087,    0.67272268907563, "#0f6333", "Sex = 'female' <br>increases average response <br>by 0.3065"), .Names = c("variable", "contribution","cummulative", "sign", "label")), structure(list("* Fare = 71",    0.0709915966386554, 0.743714285714286, "#a3142f", "Fare = 71 (low value) <br>increases average response <br>by 0.071"), .Names = c("variable","contribution", "cummulative", "sign", "label")), structure(list(    "* Pclass = 1", 0.210210084033614, 0.953924369747899, "#0f6333",    "Pclass = 1 (low value) <br>increases average response <br>by 0.2102"), .Names = c("variable", "contribution", "cummulative","sign", "label")), structure(list("* Embarked = C", 0.0145014005602241,    0.968425770308123, "#a3142f", "RF"), .Names = c("variable", "contribution","cummulative", "sign", "label")), structure(list("+ other factors",    0.0154089635854342, 0.983834733893557, "#a3142f", "All other features <br>decrease average response <br>by 0.01"), .Names = c("variable","contribution", "cummulative", "sign", "label"))), dependencies = "tooltipD3.js", css = "breakDownD3.css", options=list(xmin = 0.2, xmax = 1, model_avg = 0.3662, model_res = 0.9869)
//
// r2d3: https://rstudio.github.io/r2d3
//

b2data = [{ "variable":"+ Sex = female",      "contribution":0.3065,      "cummulative":0.6727,      "sign":"#0f6333",      "label":""   },   {        "variable":"+ Fare = 71",      "contribution":0.071,      "cummulative":0.7437,      "sign":"#0f6333",      "label":""   },   {        "variable":"+ Pclass = 1",      "contribution":0.2102,      "cummulative":0.9539,      "sign":"#0f6333",      "label":""   },   {        "variable":"+ Embarked = C",      "contribution":-0.02,      "cummulative":0.9339,      "sign":"#a3142f",      "label":"Embarked = 'C' <br>decreases average response <br>by 0.02"   },   {        "variable":"+ 3 others",      "contribution":-0.01,      "cummulative":0.9239,      "sign":"#a3142f",      "label":"All other features <br>decrease average response <br>by 0.01"}]

var x_min   = options.xmin
var x_max   = options.xmax
var lmargin = 130
var rmargin = 10
var bmargin = 50
var tmargin = 50
var w     = width - lmargin - rmargin
var h     = height - bmargin - tmargin

var yAxis  = d3.scaleLinear().domain([0, data.length + 1.5]).range([tmargin, tmargin+h])
var xAxis  = d3.scaleLinear().domain([x_min, x_max]).range([lmargin,w + lmargin])

// add OX axis to the plot
svg
  .append("g")
  .selectAll('text')
  .data(xAxis.ticks(7))
  .enter().append('text')
  .text(function(d) { return d; })
  .attr('y', h + tmargin + 20)
  .attr("x", function(d) { return xAxis(d); })
  .attr('text-anchor', 'middle')
  .style("font-family", "'Roboto Condensed', sans-serif")
  .style("fill","#878787")

// add veritical lines to the plot
svg
  .append("g")
  .selectAll('line')
  .data(xAxis.ticks(7))
  .enter().append('line')
  .attr('y1', tmargin - 10)
  .attr('y2', h + tmargin)
  .attr("x1", function(d) { return xAxis(d); })
  .attr("x2", function(d) { return xAxis(d); })
  .style("stroke","#b6b7b670")
  .style("stroke-width","1")
  .style("stroke-dasharray","4")

// baseline grouping object, here we will title and line
var baseg = svg
  .append("g")

// plot line for model average
baseg
  .append("line")
  .data([options.model_avg])
  .attr('y1', 2)
  .attr('y2', h + tmargin)
  .attr("x1", function(d) { return xAxis(d); })
  .attr("x2", function(d) { return xAxis(d); })
  .style("stroke","#b6b7b6")
  .style("stroke-width","2")

baseg
  .append("text")
  .data([options.model_avg])
  .attr("x", function(d) { return xAxis(d) - 5; })
  .attr("y", 20)
  .text("Average response")
  .style("font-family", "'Roboto Condensed', sans-serif")
  .style("font-size", "20")
  .style("fill","#878787")
  .attr("text-anchor","end")

baseg
  .append("text")
  .data([options.model_avg])
  .attr("x", function(d) { return xAxis(d) - 5; })
  .attr("y", 40)
  .text(function(d) { return d; })
  .style("font-size", "18")
  .style("font-family", "'Roboto Condensed', sans-serif")
  .style("fill","#fa415a")
  .attr("text-anchor","end")

// finalline grouping object, here we will title and line
var finalg = svg
  .append("g")

finalg
  .append("text")
  .data([options.model_res])
  .attr("x", function(d) { return xAxis(d) - 5; })
  .attr("y", function(d) { return yAxis(data.length + 1) - 10; })
  .text("Individual response")
  .style("font-family", "'Roboto Condensed', sans-serif")
  .style("font-size", "20")
  .style("fill","#878787")
  .attr("text-anchor","end")

finalg
  .append("text")
  .data([options.model_res])
  .attr("x", function(d) { return xAxis(d) - 5; })
  .attr("y", function(d) { return yAxis(data.length+1)+10; })
  .text(function(d) { return d; })
  .style("font-family", "'Roboto Condensed', sans-serif")
  .style("font-size", "18")
  .style("fill","#fa415a")
  .attr("text-anchor","end")

// main group, there we will plot all rectangles
var maing = svg
  .append("g")

// tooltips
var tool_tip = d3.tip()
      .attr("class", "d3-tip")
      .offset([-8, 0])
      .html(function(d) { return d.label; });

maing
  .call(tool_tip);

// bars
maing
  .selectAll("rect")
  .data(data)
  .enter()
  .append("rect")
  .attr("x", function(d, i) {
      return (xAxis(d.cummulative - Math.max(d.contribution, 0)));
    })
  .attr("y", function(d, i) { return yAxis(i) })
  .attr("width", function(d, i) {
      return (Math.abs(xAxis(d.contribution) - xAxis(0)));
    })
  .attr("height", yAxis(1) - yAxis(0) - 2)
  .style("fill", function(d, i) {
      return d.sign;
    })
    .on('mouseover', tool_tip.show)
    .on('mouseout', tool_tip.hide);

svg
  .append("g")
  .selectAll("text")
  .data(data)
  .enter()
  .append("text")
  .attr("x", 0)
  .attr("y", function(d, i) { return yAxis(i+0.7) })
  .style("font-family", "'Roboto Condensed', sans-serif")
  .style("fill","#878787")
  .style("font-size", "20")
  .text(function(d) {
      return (d.variable);
    })

svg
  .append("g")
  .selectAll("line")
  .data(data)
  .enter()
  .append("line")
  .attr("x1", function(d, i) {
      return (xAxis(d.cummulative));
    })
  .attr("x2", function(d, i) {
      return (xAxis(d.cummulative));
    })
  .attr("y1", function(d, i) { return yAxis(i) })
  .attr("y2", function(d, i) { return yAxis(i + 2)-2 })
  .style("stroke","black")
  .style("stroke-width","1")
