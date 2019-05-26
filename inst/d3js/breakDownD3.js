var minValue   = options.xmin,
    maxValue = options.xmax,
    n = options.n, m = options.m,
    barWidth = options.barWidth;

var margin = {top: 98, right: 30, bottom: 71, left: 120, inner: 42},
    w = width - margin.left - margin.right,
    h = height - margin.top - margin.bottom,
    labelsMargin = margin.left - 8,
    plotTop = margin.top,
    plotHeight = m*barWidth + (m+1)*barWidth/2,
    plotWidth = 420;

var chartTitle = "Local attributions"; // subject to change

if (options.scaleHeight === true) {
  if (h > n*plotHeight + (n-1)*margin.inner) {
    var temp = h - n*plotHeight - (n-1)*margin.inner;
    plotTop += temp/2;
  }
}

var colors = getColors(n, "breakDown"),
    positiveColor = colors[0],
    negativeColor = colors[1],
    defaultColor = colors[2];

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
        .attr("transform","translate(" + (yGridStart-8) + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());

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

  var bars = svg.selectAll()
        .data(bData)
        .enter()
        .append("g");

  var tool_tip = d3.tip()
        .attr("class", "tooltip")
        .offset([-8, 0])
        .html(function(d) { return changedTooltipHtml(null); });

  svg.call(tool_tip);

  let intercept = bData[0].cummulative;
  let prediction = bData[m-1].cummulative;

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

  plotTop += (margin.inner + plotHeight);
}

function changedTooltipHtml(temp) {
  return null;
}
