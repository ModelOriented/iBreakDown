// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
  // r2d3::r2d3("~/Desktop/r2d3.js", data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20))

var barHeight = Math.floor(height / data.length);
var g1 = svg.append('g')

g1.selectAll('rect')
.data(data)
.enter().append('rect')
.attr('width', function(d) { return d * width; })
.attr('height', barHeight - 2)
.attr('y', function(d, i) { return i * barHeight; })
.attr('fill', 'black')
.on("mouseover", function(d) {
  d3.select(this)
  .transition()
  .duration(1000)
  .attr('width', 0)
  .style("fill", "red");
})
.on("mouseout", function(d) {
  d3.select(this)
  .transition()
  .duration(1000)
  .attr('width', function(d) { return d * width; })
  .style("fill", "black");
})
