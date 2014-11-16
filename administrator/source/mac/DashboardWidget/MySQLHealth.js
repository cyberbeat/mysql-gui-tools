

var serverHost= "localhost:3306";
var username = "root";
var password = "";
var graphType1 = 1;
var graphType2 = 0;
var secondsPerTick = 1;

var colors = new Array("#ffffff", "#88ddff", "#8888ff");
var ticks = 0;
var secTicks = 0;
var lastTime = 0;
var maxValueLimits = new Array(1024*1024*1024, 1024*1024*100, 1024*1024*10, 1024*1024*5, 1024*1024, 100*1024, 50*1024, 32*1024, 16*1024, 8*1024, 4*1024, 2048, 1024, 512, 300, 200, 100, 50, 10, 5);
var graphs;

function makeKey(key)
{
	return widget.identifier + "-" + key;
}

function Graph(type, color) 
{
    // construct
    this.data = new Array();
    this.lastValue = 0;
    this.maxValue = 0;
    this.title = "";
    this.color = color;
    this.fmtDecimals = 0;
    this.fmtScale = 1;
    this.type = type;
    this.variable = "";
    this.statCur = 0;
    this.statMin = 0;
    this.statMax = 0;
    this.statAvg = 0;

    // methods
    this.render = function(ctx)
    {
        ctx.strokeStyle=this.color;
        ctx.moveTo(340-this.data.length*2 + 0.5, 80-(this.data[0] * 100.0 / this.maxValue)*80/100 + 0.5);
        for (var i= 1; i < this.data.length; i++) {
            var x = this.data[i] * 100.0 / this.maxValue;
            ctx.lineTo((341-this.data.length*2)+i*2 + 0.5, 80-Math.round(x * 80/100) + 0.5);
        }
        ctx.stroke();
    }

    this.colorText = function(text)
    {
        return "<font color='"+this.color+"'>"+text+"</font>";
    }

    this.formatNumber = function(value)
    {
        value = value / this.fmtScale;

        if (this.fmtDecimals == 0 || value == 0)
            value = Math.round(value);
        else
            value = value.toFixed(this.fmtDecimals);

        switch (this.fmtScale)
        {
        case 1: return this.colorText(value);
        case 1024: return this.colorText(value+"K");
        case 1024*1024: return this.colorText(value+"M");
        }
        return "";
    }

    this.changeScale = function(value)
    {
        if (value > 1024*1024)
        {
            this.fmtDecimals = 2;
            this.fmtScale = 1024*1024;
        }
        else if (value > 1024)
        {
            this.fmtDecimals = 1;
            this.fmtScale = 1024;
        }
        else
        {
            this.fmtDecimals = 0;
            this.fmtScale = 1;
        }
    }

    this.formatStat = function(stat)
    {
        var value;

        switch (stat)
        {
        case 0: value = this.statCur; break;
        case 1: value = this.statMin; break;
        case 2: value = this.statMax; break;
        case 3: value = this.statAvg; break;
        }
        return this.formatNumber(value);
    }

    this.formatTitle = function()
    {
        return this.colorText(this.title);
    }

    this.tick = function() 
    {
    }
}


function AbsGraph(type, color)
{
    this.inheritFrom = Graph;
    this.inheritFrom(type, color);

    switch (type)
    {
    case 1: 
        this.title = "Connections";
        this.maxValue= 10;
        this.variable = "Threads_connected"; 
        this.lastValue= 0;
        this.fmtScale= 1;
        this.fmtDecimals = 0;
        break;
    default:
        alert("Unknown graph type "+type);
    }

    this.tick = function()
    {
        var count;

        switch (type)
        {
        case 1:
            count= parseFloat(MySQLMonitor.getStatus(this.variable));
            break;
        }

        if (count >= this.maxValue)
        {
            for (var i= maxValueLimits.length-1; i >= 0; i--)
            {
                if (count < maxValueLimits[i])
                {
                    this.maxValue= maxValueLimits[i];
                    break;
                }
            }
            this.changeScale(this.maxValue);
        }

        if (this.data.length > 0)
        {
            if (count < this.statMin || this.statMin == -1) this.statMin= count;
            if (count > this.statMax) this.statMax= count;
            this.statAvg= (this.statAvg * ticks + count) / (ticks + 1);
            this.statCur= count;
            this.data.push(count);
        }
        else
        {
            this.statMin= -1;
            this.statMax= count;
            this.statAvg= count;
            this.statCur= count;
            this.data.push(0);
        }

        if (this.data.length >= 340/2)
            this.data.shift();
    }
}


function DeltaGraph(type, color)
{
    this.inheritFrom = Graph;
    this.inheritFrom(type, color);

    this.starting = 1;
    this.scaleChangeCounter = 0;

    switch (type)
    {
    case 2:
        this.title = "QCache Hitrate";
        this.maxValue= 100;
        this.variable= "Qcache_hitrate";
        this.lastValue= 0;
        this.fmtScale= 1;
        this.fmtDecimals = 0;
        break;

    case 5:
      this.title = "Selects";
      this.maxValue= 100;
      this.variable= "Com_select";
      this.lastValue= 0;
      this.fmtScale= 1;
      this.fmtDecimals = 0;
      break;
    case 6:
      this.title = "Inserts";
      this.maxValue= 100;
      this.variable= "Com_insert";
      this.lastValue= 0;
      this.fmtScale= 1;
      this.fmtDecimals = 0;
      break;
    case 7:
      this.title = "Updates";
      this.maxValue= 100;
      this.variable= "Com_update";
      this.lastValue= 0;
      this.fmtScale= 1;
      this.fmtDecimals = 0;
      break;
    case 8:
      this.title = "Deletes";
      this.maxValue= 100;
      this.variable= "Com_delete";
      this.lastValue= 0;
      this.fmtScale= 1;
      this.fmtDecimals = 0;
      break;
      
    case 10:
        this.title = "Queries";
        this.maxValue= 100;
        this.variable= "Questions";
        this.lastValue= 0;
        this.fmtScale= 1;
        this.fmtDecimals = 0;
        break;
    case 11:
        this.title = "Bytes In";
        this.maxValue= 5;
        this.variable= "Bytes_received";
        this.lastValue= 0;
        this.fmtScale= 1;
        this.fmtDecimals = 1;
        break;
    case 12:
        this.title = "Bytes Out";
        this.maxValue= 5;
        this.variable= "Bytes_sent";
        this.lastValue= 0;
        this.fmtScale= 1;
        this.fmtDecimals = 1;
        break;
    }

    this.tick = function()
    {
        var value= MySQLMonitor.getStatus(this.variable);
        var count= parseFloat(value);
        var delta;

        if (this.starting == 0)
            delta= count - this.lastValue;
        else
            delta= 0;
        this.lastValue= count;

        delta = (delta / secondsPerTick) / this.fmtScale;

       if (this.starting == 0)
        {
            if (delta < this.statMin || this.statMin == -1) this.statMin= delta;
            if (delta > this.statMax) this.statMax= delta;
            this.statAvg= (this.statAvg * ticks + delta) / (ticks + 1);
            this.statCur= delta;
            this.data.push(delta);
        }
        else
        {
            this.statMin= -1;
            this.statMax= delta;
            this.statAvg= delta;
            this.statCur= delta;
            this.data.push(0);
        }

        if (this.scaleChangeCounter <= 0)
        {
            for (var i= 0; i < maxValueLimits.length; i++)
            {
                if (this.statAvg > maxValueLimits[i])
                {
                    this.maxValue= maxValueLimits[i];
                    break;
                }
            }
            this.scaleChangeCounter = 10;
            this.changeScale(this.maxValue);
        }
        this.scaleChangeCounter--;

        this.starting = 0;
        if (this.data.length >= 340/2)
            this.data.shift();
    }
}



function drawGraphs()
{
        var canvas = document.getElementById("mycanvas");
        var ctx = canvas.getContext("2d");
        var top, mid, bot;

        ctx.save();
        ctx.fillStyle="#000000";
        ctx.fillRect(0, 0, 340, 80);
   
        ctx.strokeStyle="#203038";
        for (var i= 0; i <= 10; i++)
        {
                ctx.moveTo(0, Math.round(i*80/10)+0.5);
                ctx.lineTo(340, Math.round(i*80/10)+0.5);
        }
        ctx.stroke();
        for (var i= 1; i <= 35; i++)
        {
                var x= i*10 - (ticks*2) % 10;
                if (x > 0 && x < 340)
                {
                        ctx.moveTo(x + 0.5, 0);
                        ctx.lineTo(x + 0.5, 80);
                }
        }
        ctx.stroke();
 
        top = ""
        mid = ""
        bot = ""
        for (var g = 0; g < graphs.length; g++)
        {
            graphs[g].render(ctx);
            if (g > 0)
            {
                top += "/";
                mid += "/";
                bot += "/";
            }
            top += graphs[g].formatNumber(graphs[g].maxValue);
            mid += graphs[g].formatNumber(graphs[g].maxValue/2);
            bot += graphs[g].formatNumber(0);
       }
        document.getElementById("scaleTop").innerHTML = top;
        document.getElementById("scaleMid").innerHTML = mid;
        document.getElementById("scaleBot").innerHTML = bot;
 
        ctx.restore();
}


function formatStat(stat)
{ 
        var tmp = "";
        
        for (var i= 0; i < graphs.length; i++)
        {
                if (tmp != "") tmp+= "/";
                tmp+= graphs[i].formatStat(stat);
        }
        return tmp;
}


function updateStats()
{
        document.getElementById("curValue").innerHTML = "Cur: "+formatStat(0);
        document.getElementById("minValue").innerHTML = "Min: "+formatStat(1);
        document.getElementById("maxValue").innerHTML = "Max: "+formatStat(2);
        document.getElementById("medValue").innerHTML = "Avg: "+formatStat(3);
}


function doTick()
{
    if (secTicks % secondsPerTick == 0)
    {
        MySQLMonitor.queryStatus();
      
        for (var i = 0; i < graphs.length; i++)
        {
            graphs[i].tick();
        }
        ticks++;

        drawGraphs();
        updateStats();
    }
    secTicks++;
    clearTimeout();
    setTimeout("doTick();", 1000);
}


function createGraph(type)
{       
        type= parseInt(type);
        if (type < 10)
                return new AbsGraph(type, colors[graphs.length]);
        else
                return new DeltaGraph(type, colors[graphs.length]);
}

function setupGraphs()
{
    var type;

    document.getElementById("header").innerHTML = "MySQL @ "+ document.getElementById("host").value;
    document.getElementById("curValue").innerHTML = "Cur: ";
    document.getElementById("minValue").innerHTML = "Min: ";
    document.getElementById("maxValue").innerHTML = "Max: ";
    document.getElementById("medValue").innerHTML = "Avg: ";


    graphs = new Array();
    type= document.getElementById("graphType1").value;
    if (type != 0)
        graphs.push(createGraph(type));

    type= document.getElementById("graphType2").value;
    if (type != 0)
        graphs.push(createGraph(type));
    if (graphs.length > 1)
        document.getElementById("info").innerHTML = graphs[0].formatTitle() + "/" + graphs[1].formatTitle();
    else if (graphs.length > 0)
        document.getElementById("info").innerHTML = graphs[0].formatTitle();
    ticks= 0;
}


function changeGraph(which, select)
{
}


function savePrefs()
{
    var changed = 0;
    var changedConnect = 0;

    widget.setPreferenceForKey(document.getElementById("host").value, makeKey("host"));
    widget.setPreferenceForKey(document.getElementById("user").value, makeKey("user"));
    widget.setPreferenceForKey(document.getElementById("pass").value, makeKey("pass"));
    widget.setPreferenceForKey(document.getElementById("graphType1").value, makeKey("graphType1"));
    widget.setPreferenceForKey(document.getElementById("graphType2").value, makeKey("graphType2"));
    widget.setPreferenceForKey(document.getElementById("delay").value, makeKey("delay"));

    if (serverHost != document.getElementById("host").value)
    {
        serverHost = document.getElementById("host").value;
        changed = 1;
        changedConnect = 1;
    }
    if (username != document.getElementById("user").value)
    {
        username = document.getElementById("user").value;
        changed = 1;
        changedConnect = 1;
    }
    if (password != document.getElementById("pass").value)
    {
        password = document.getElementById("pass").value;
        changed = 1;
        changedConnect = 1;
    }
    if (graphType1 != parseInt(document.getElementById("graphType1").value))
    {
        graphType1 = parseInt(document.getElementById("graphType1").value);
        changed = 1;
    }
    if (graphType2 != parseInt(document.getElementById("graphType2").value))
    {
        graphType2 = parseInt(document.getElementById("graphType2").value);
        changed = 1;
    }
    if (secondsPerTick != parseInt(document.getElementById("delay").value))
    {
        secondsPerTick = parseInt(document.getElementById("delay").value);
        if (secondsPerTick < 1)
            secondsPerTick = 1;
        changed = 1;
    }
    
    if (changed)
    {
        if (changedConnect)
        {
            if (MySQLMonitor.connect(serverHost, username, password))
            {
                document.getElementById("error").style.display = "none";
                setupGraphs();
            }
            else
            {
                document.getElementById("error").style.display = "block";
                document.getElementById("error").innerHTML = "Error connecting to "+serverHost;
            }
        }
        else
            setupGraphs();
        doTick();
    }
}


function displayPrefs()
{
    //serverHost= widget.preferenceForKey(makeKey("host"));
    document.getElementById("host").value = serverHost;
    document.getElementById("user").value = username;
    document.getElementById("pass").value = password;
    document.getElementById("graphType1").value = graphType1;
    document.getElementById("graphType2").value = graphType2;
    document.getElementById("delay").value = secondsPerTick;

    document.getElementById("testResult").innerHTML = "";
}


function testConnection()
{
    var  result = MySQLMonitor.testConnection(document.getElementById("host").value,
                                                document.getElementById("user").value,
                                                document.getElementById("pass").value);
    if (result == "")
        document.getElementById("testResult").innerHTML = "<font color=white><bold>OK</bold></font>";
    else
        document.getElementById("testResult").innerHTML = "<font color=red><bold>Connect Error</bold></font>";
}


function loaded()
{
  createGenericButton(document.getElementById('doneButton'), 'Done', hidePrefs);
  createGenericButton(document.getElementById('testButton'), 'Test Connection', testConnection);

  serverHost= widget.preferenceForKey(makeKey("host"));
  if(serverHost == undefined)
    serverHost= "localhost:3306";
  document.getElementById("host").value = serverHost;

  username= widget.preferenceForKey(makeKey("user"));
  if(username == undefined)
    username= "root";
  document.getElementById("user").value = username;

  password= widget.preferenceForKey(makeKey("pass"));
  if(password == undefined)
    password= "";
  document.getElementById("pass").value = password;

  graphType1= widget.preferenceForKey(makeKey("graphType1"));
  if(graphType1 == undefined)
    graphType1= 1;
  document.getElementById("graphType1").value = graphType1;

  graphType2= widget.preferenceForKey(makeKey("graphType2"));
  if(graphType2 == undefined)
    graphType2= 0;
  document.getElementById("graphType2").value = graphType2;

  secondsPerTick= graphType2= widget.preferenceForKey(makeKey("delay"));
  if(secondsPerTick == undefined)
    secondsPerTick= 1;
  document.getElementById("delay").value = secondsPerTick;

  if (MySQLMonitor)
  {
    if (MySQLMonitor.connect(serverHost, username, password))
    {
      document.getElementById("error").style.display = "none";
      setupGraphs();
      MySQLMonitor.queryStatus();
      // does not work
      //setInterval("doTick();", 1000);
      doTick();
    }
    else
    {
      document.getElementById("error").style.display = "block";
      document.getElementById("error").innerHTML = "Error connecting to "+serverHost;
    }
  }
  else
  {
    alert("MySQL Monitor Plugin not loaded");
  }
}


//=====================================================================================================================

function showPrefs()
{        
        var front = document.getElementById("front");
        var back = document.getElementById("back");

        if (window.widget)
                widget.prepareForTransition("ToBack");          // freezes the widget so that you can change it without the user noticing

        front.style.display="none";             // hide the front
        back.style.display="block";             // show the back        
        if (window.widget)
                setTimeout ('widget.performTransition();', 0);          // and flip the widget over     

        document.getElementById('fliprollie').style.display = 'none';  // clean up the front side - hide the circle behind the info button

        displayPrefs();
}


function hidePrefs()
{
        var front = document.getElementById("front");
        var back = document.getElementById("back");

        if (window.widget)
                widget.prepareForTransition("ToFront");         // freezes the widget and prepares it for the flip back to the front
        back.style.display="none";                      // hide the back
        front.style.display="block";            // show the front

        if (window.widget)
                setTimeout('widget.performTransition();', 0);          // and flip the widget back to the front

        savePrefs();
}



var flipShown = false;          // a flag used to signify if the flipper is currently shown or not.


// A structure that holds information that is needed for the animation to run.
var animation = {duration:0, starttime:0, to:1.0, now:0.0, from:0.0, firstElement:null, timer:null};

function mousemove (event)
{
        if (!flipShown)                 // if the preferences flipper is not already showing...
        {
                if (animation.timer != null)                    // reset the animation timer value, in case a value was left behind
                {
                        clearInterval (animation.timer);
                        animation.timer  = null;
                }

                var starttime = (new Date).getTime() - 13;              // set it back one frame

                animation.duration = 500;                                                                                               // animation time, in ms
                animation.starttime = starttime;                                                                                // specify the start time
                animation.firstElement = document.getElementById ('flip');              // specify the element to fade
                animation.timer = setInterval ("animate();", 13);                                               // set the animation function
                animation.from = animation.now;                                                                                 // beginning opacity (not ness. 0)
                animation.to = 1.0;                                                                                                             // final opacity
                animate();                                                                                                                              // begin animation
                flipShown = true;                                                                                                               // mark the flipper as animated
        }
}



function mouseexit (event)
{
        if (flipShown)
        {
                // fade in the flip widget
                if (animation.timer != null)
                {
                        clearInterval (animation.timer);
                        animation.timer  = null;
                }

                var starttime = (new Date).getTime() - 13;

                animation.duration = 500;
                animation.starttime = starttime;
                animation.firstElement = document.getElementById ('flip');
                animation.timer = setInterval ("animate();", 13);
                animation.from = animation.now;
                animation.to = 0.0;
                animate();
                flipShown = false;
        }
}


function animate()
{
        var T;
        var ease;
        var time = (new Date).getTime();


        T = limit_3(time-animation.starttime, 0, animation.duration);

        if (T >= animation.duration)
        {
                clearInterval (animation.timer);
                animation.timer = null;
                animation.now = animation.to;
        }
        else
        {
                ease = 0.5 - (0.5 * Math.cos(Math.PI * T / animation.duration));
                animation.now = computeNextFloat (animation.from, animation.to, ease);
        }

        animation.firstElement.style.opacity = animation.now;
}



function limit_3 (a, b, c)
{
    return a < b ? b : (a > c ? c : a);
}

function computeNextFloat (from, to, ease)
{
    return from + (to - from) * ease;
}

// these functions are called when the info button itself receives onmouseover and onmouseout events

function enterflip(event)
{
        document.getElementById('fliprollie').style.display = 'block';
}

function exitflip(event)
{
        document.getElementById('fliprollie').style.display = 'none';
}

