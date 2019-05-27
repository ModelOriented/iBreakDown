var minValue   = options.xmin,
    maxValue = options.xmax,
    n = options.n, m = options.m,
    barWidth = options.barWidth;

// effort to make labels margin
var temp = svg.selectAll()
              .data(data[1])
              .enter();

var textWidth = [];

temp.append("text")
    .text(function(d) { return d;})
    .style("font-size", "11px")
    .each(function(d,i) {
        var thisWidth = this.getComputedTextLength();
        textWidth.push(thisWidth);
    });

svg.selectAll('text').remove();
temp.remove();

var maxLength = d3.max(textWidth)+10;
////

var margin = {top: 98, right: 30, bottom: 71, left: maxLength, inner: 42},
    w = width - margin.left - margin.right,
    h = height - margin.top - margin.bottom,
    plotTop = margin.top,
    plotHeight = m*barWidth + (m+1)*barWidth/2,
    plotWidth = 420*1.3;

var chartTitle = "Local attributions"; // subject to change

if (options.scaleHeight === true) {
  if (h > n*plotHeight + (n-1)*margin.inner) {
    var temp = h - n*plotHeight - (n-1)*margin.inner;
    plotTop += temp/2;
  }
}

if (options.vcolors === "default") {
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

function breakDown(data){
  var barData = data[0];
  var modelNames = Object.keys(barData);

  for (let i=0; i<n; i++){
      let modelName = modelNames[i];
      singlePlot(modelName, barData[modelName], i+1);
  }
}

function singlePlot(modelName, bData, i){

  var x = d3.scaleLinear()
        .range([margin.left,  margin.left + plotWidth])
        .domain([minValue, maxValue]);

  if (i == n){

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
        .domain(bData.map(function (d) {
             return d.variable;
        }));

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

  if (i == 1){
    svg.append("text")
          .attr("x", yGridStart)
          .attr("y", plotTop - 60)
          .attr("class", "bigTitle")
          .text(chartTitle);
  }

  // add tooltip
  var tool_tip = d3.tip()
        .attr("class", "tooltip")
        .offset([-8, 0])
        .html(function(d) { return changedTooltipHtml(d); });

  svg.call(tool_tip);

  // find boundaries
  let intercept = bData[0].barSupport;
  let prediction = bData[m-1].cummulative;

  // make dotted line from intercept to prediction
  var dotLineData = [{"x": x(intercept), "y": y("intercept")},
                     {"x": x(intercept), "y": y("prediction") + barWidth}];

  var lineFunction = d3.line()
                         .x(function(d) { return d.x; })
                         .y(function(d) { return d.y; });
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
        .attr("class", modelName.replace(/\s/g,''))
        .attr("fill",function(d){
          switch(d.sign){
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
        .attr("x", function(d){
          switch(d.sign){
            case "X":
              return intercept<prediction ? x(intercept) : x(prediction);
            default:
              return x(d.barStart);
          }
        })
        .attr("width", function(d){
          switch(d.sign){
            case "-1":
              return x(d.barSupport) - x(d.barStart);
            case "1":
              return x(d.barSupport) - x(d.barStart);
            case "X":
              return intercept<prediction ? x(prediction)-x(intercept) : x(intercept)-x(prediction);
            default:
              return 0;
          }
        })
        .on('mouseover', tool_tip.show)
        .on('mouseout', tool_tip.hide);

  // add labels to bars
  var contributionLabel = svg.selectAll()
        .data(bData)
        .enter()
        .append("g");

  contributionLabel.append("text")
        .attr("x", d => {
          switch(d.sign){
            case "X":
              return intercept<prediction ? x(prediction) + 5 : x(prediction) - 5;
            default:
              return x(d.barSupport) + 5;
          }
        })
        .attr("text-anchor", d => {
          if (d.sign === "X" && intercept > prediction) return "end";
        })
        .attr("y", d => y(d.variable) + barWidth*3/4)
        .attr("class", "axisLabel")
        .text(d => {
          switch(d.variable){
            case "intercept":
            case "prediction":
              return d.cummulative;
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
        .attr("x1", d => d.sign == "-1" ? x(d.barStart) : x(d.barSupport))
        .attr("y1", d => y(d.variable))
        .attr("x2", d => d.sign == "-1" ? x(d.barStart) : x(d.barSupport))
        .attr("y2", d => d.sign == "X" ? y(d.variable) : y(d.variable) + barWidth*2.5);

  // update plotTop
  plotTop += (margin.inner + plotHeight);
}

function changedTooltipHtml(d, prediction) {
  var temp = "<center>";
  temp += d.label;
  temp += "</center>";
  return temp;
}
