var xMin = options.xmin,
    xMax = options.xmax,
    n = options.n, m = options.m,
    barWidth = options.barWidth,
    scaleHeight = options.scaleHeight,
    vColors = options.vcolors,
    chartTitle = options.chartTitle;

var time = options.time;
/// prevent plot from reloading onResize
if (!options.reload) {
  r2d3.onResize(function() {
    return;
  });
}

var maxLength = calculateTextWidth(data[1])+15;

var margin = {top: 78, right: 30, bottom: 71, left: maxLength, inner: 42},
    h = height - margin.top - margin.bottom,
    plotTop = margin.top,
    plotHeight = m*barWidth + (m+1)*barWidth/2,
    plotWidth = 420*1.2;

if (scaleHeight === true) {
  if (h > n*plotHeight + (n-1)*margin.inner) {
    var temp = h - n*plotHeight - (n-1)*margin.inner;
    plotTop += temp/2;
  }
}

if (vColors === "default") {
  var colors = getColors(n, "breakDown"),
    positiveColor = colors[0],
    negativeColor = colors[1],
    defaultColor = colors[2];
} else {
  var colors = options.vcolors,
    positiveColor = colors[0],
    negativeColor = colors[1],
    defaultColor = colors[2];
}

breakDown(data);

// change font
svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

function breakDown(data) {
  var barData = data[0];
  var modelNames = Object.keys(barData);

  for (let i=0; i<n; i++) {
      let modelName = modelNames[i];
      singlePlot(modelName, barData[modelName], i+1);
  }
}

function singlePlot(modelName, bData, i) {

  var x = d3.scaleLinear()
        .range([margin.left,  margin.left + plotWidth])
        .domain([xMin, xMax]);

  if (i == n) {

    var xAxis = d3.axisBottom(x)
                .ticks(5)
                .tickSize(0);

    xAxis = svg.append("g")
            .attr("class", "axisLabel")
            .attr("transform", "translate(0," + (plotTop + plotHeight) + ")")
            .call(xAxis)
            .call(g => g.select(".domain").remove());
  }

  var y = d3.scaleBand()
        .rangeRound([plotTop, plotTop + plotHeight])
        .padding(0.33)
        .domain(bData.map(d => d.variable));

  var xGrid = svg.append("g")
         .attr("class", "grid")
         .attr("transform", "translate(0," + (plotTop + plotHeight) + ")")
         .call(d3.axisBottom(x)
                .ticks(10)
                .tickSize(-plotHeight)
                .tickFormat("")
        ).call(g => g.select(".domain").remove());

  // effort to make grid endings clean
  let str = xGrid.select('.tick:first-child').attr('transform');
  let yGridStart = str.substring(str.indexOf("(")+1,str.indexOf(","));
  str = xGrid.select('.tick:last-child').attr('transform');
  let yGridEnd = str.substring(str.indexOf("(")+1,str.indexOf(","));

  var yGrid = svg.append("g")
         .attr("class", "grid")
         .attr("transform", "translate(" + yGridStart + ",0)")
         .call(d3.axisLeft(y)
                .tickSize(-(yGridEnd-yGridStart))
                .tickFormat("")
        ).call(g => g.select(".domain").remove());

  var yAxis = d3.axisLeft(y)
        .tickSize(0);

  yAxis = svg.append("g")
        .attr("class", "axisLabel")
        .attr("transform","translate(" + (yGridStart-10) + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());

  yAxis.select(".tick:last-child").select("text").attr('font-weight', 600);

  svg.append("text")
        .attr("x", yGridStart)
        .attr("y", plotTop - 15)
        .attr("class", "smallTitle")
        .text(modelName);

  if (i == 1) {
    svg.append("text")
          .attr("x", yGridStart)
          .attr("y", plotTop - 40)
          .attr("class", "bigTitle")
          .text(chartTitle);
  }

  // add tooltip
  var tool_tip = d3.tip()
        .attr("class", "d3-tip")
        .html(d => staticTooltipHtml(d));

  svg.call(tool_tip);

  // find boundaries
  let intercept = bData[0].contribution > 0 ? bData[0].barStart : bData[0].barSupport;

  // make dotted line from intercept to prediction
  var dotLineData = [{"x": x(intercept), "y": y("intercept")},
                     {"x": x(intercept), "y": y("prediction") + barWidth}];

  var lineFunction = d3.line()
                         .x(d => d.x)
                         .y(d => d.y);
  svg.append("path")
        .data([dotLineData])
        .attr("class", "dotLine")
        .attr("d", lineFunction)
        .style("stroke-dasharray", ("1, 2"));

  // add bars
  var bars = svg.selectAll()
        .data(bData)
        .enter()
        .append("g");

  bars.append("rect")
        .attr("fill", function(d) {
          switch (d.sign) {
            case "-1":
              return negativeColor;
            case "1":
              return positiveColor;
            default:
              return defaultColor;
          }
        })
        .attr("y", d => y(d.variable) )
        .attr("height", y.bandwidth() )
        .attr("x", d => d.contribution > 0 ? x(d.barStart) : x(d.barSupport))
        .on('mouseover', tool_tip.show)
        .on('mouseout', tool_tip.hide)
        .transition()
        .duration(time)
        .delay((d,i) => i * time)
        .attr("width", d => x(d.barSupport) - x(d.barStart))
        .attr("x", d => x(d.barStart));

  // add labels to bars
  var contributionLabel = svg.selectAll()
        .data(bData)
        .enter()
        .append("g");

  contributionLabel.append("text")
        .attr("x", d => {
          switch (d.sign) {
            case "X":
              return d.contribution < 0 ? x(d.barStart) - 5 : x(d.barSupport) + 5;
            default:
              return x(d.barSupport) + 5;
          }
        })
        .attr("text-anchor", d => d.sign == "X" && d.contribution < 0 ? "end" : null)
        .attr("y", d => y(d.variable) + barWidth/2)
        .attr("class", "axisLabel")
        .attr("dy", "0.4em")
        .transition()
        .duration(time)
        .delay((d,i) => (i+1) * time)
        .text(d => {
          switch (d.variable) {
            case "intercept":
            case "prediction":
              return d.cumulative;
            default:
              return d.sign === "-1" ? d.contribution : "+"+d.contribution;
          }
        });

  // add lines to bars
  var lines = svg.selectAll()
        .data(bData)
        .enter()
        .append("g");

  lines.append("line")
        .attr("class", "interceptLine")
        .attr("x1", d => d.contribution < 0 ? x(d.barStart) : x(d.barSupport))
        .attr("y1", d => y(d.variable))
        .attr("x2", d => d.contribution < 0 ? x(d.barStart) : x(d.barSupport))
        .attr("y2", d => y(d.variable))
        .transition()
        .duration(time)
        .delay((d,i) => (i+1) * time)
        .attr("y2", d => d.variable == "prediction" ? y(d.variable) : y(d.variable) + barWidth*2.5);

  // update plotTop
  plotTop += (margin.inner + plotHeight);
}

function staticTooltipHtml(d) {
  var temp = "<center>";
  temp += d.tooltipText;
  temp += "</center>";
  return temp;
}
