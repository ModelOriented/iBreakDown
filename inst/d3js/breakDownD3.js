var minValue   = options.xmin, maxValue = options.xmax,
    n = options.n, m = options.m, barWidth = options.barWidth;

var margin = {top: 98, right: 30, bottom: 71, left: 120, inner: 42},
    w = width - margin.left - margin.right,
    h = height - margin.top - margin.bottom,
    labelsMargin = margin.left - 8,
    plotTop = margin.top,
    plotHeight = m*barWidth + (m+1)*barWidth/2,
    plotBottom = margin.top + plotHeight;

if (options.scaleHeight === true) {
  if (h > n*plotHeight + (n-1)*margin.inner) {
    var temp = h - n*plotHeight - (n-1)*margin.inner;
    plotTop += temp/2;
    plotBottom += temp/2;
  }
}

breakDown(data);

// change font
svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

function breakDown(data){

  for (let i=0; i<n; i++){
      //let variableName = variableNames[i];
      //singlePlot(variableName, profData[variableName], obsData, i+1);
  }
}

function singlePlot(){

}
/*
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
  .attr('y1', tmargin - 5)
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

// tooltips set class for correct rendering (with CSS)
var tool_tip = d3.tip()
      .attr("class", "d3-tip")
      .offset([-8, 0])
      .html(function(d) { return d.label; });

maing
  .call(tool_tip);

// bars - main point of a plot
maing
  .selectAll("rect")
  .data(data)
  .enter()
  .append("rect")
  .attr("x", function(d) {  return xAxis(d.cummulative - Math.max(d.contribution, 0)); })
  .attr("y", function(d, i) { return yAxis(i) })
  .attr("width", function(d, i) { return Math.abs(xAxis(d.contribution) - xAxis(0)); })
  .attr("height", yAxis(1) - yAxis(0) - 2)
  .style("fill", function(d) { return d.sign; })
    .on('mouseover', tool_tip.show)
    .on('mouseout', tool_tip.hide);

// add names of variables
svg
  .append("g")
  .selectAll("text")
  .data(data)
  .enter()
  .append("text")
  .attr("x", 0)
  .attr("y", function(d, i) { return yAxis(i + 0.7) })
  .style("font-family", "'Roboto Condensed', sans-serif")
  .style("fill","#878787")
  .style("font-size", "20")
  .text(function(d) { return d.variable; })

// add small links between rectangles
svg
  .append("g")
  .selectAll("line")
  .data(data)
  .enter()
  .append("line")
  .attr("x1", function(d) { return xAxis(d.cummulative); })
  .attr("x2", function(d) { return xAxis(d.cummulative); })
  .attr("y1", function(d, i) { return yAxis(i) })
  .attr("y2", function(d, i) { return yAxis(i + 2)-2 })
  .style("stroke","black")
  .style("stroke-width","1")
*/
