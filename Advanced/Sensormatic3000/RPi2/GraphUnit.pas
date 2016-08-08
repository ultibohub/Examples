unit GraphUnit;

{$mode delphi}{$H+}

{ The graph unit which encapsulates all of the details of displaying the graph }
{ both locally and via the web interface                                       }

interface

uses
  GlobalConfig,
  GlobalConst,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  HTTP,
  Font,
  Console,
  GraphicsConsole,
  SensorUnit;

  
type
 {A new class descended from THTTPDocument which allows us to create
  dynamic web content in response to a request to the web server}
 TGraphPage = class(THTTPDocument)
  constructor Create(AData:TSensorData);
  
 private
  FData:TSensorData;
  
  procedure AddHeader(AResponse:THTTPServerResponse);
  procedure AddFooter(AResponse:THTTPServerResponse);
  procedure AddContent(AResponse:THTTPServerResponse;const AContent:String);
  
 protected
  {We need to override the DoGet method in order to return our content. There are also DoPost, DoPut, DoHead etc}
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
  
 end;
 
 
 {A class to encapsulate the functionality of drawing the graph on the screen}
 TGraphWindow = class(TObject)
  constructor Create;
  destructor Destroy; override;
  
 private
  FFont:THandle;
  FHandle:THandle;
  FWidth:LongWord;
  FHeight:LongWord;
  
  FGraphTop:LongWord;
  FGraphLeft:LongWord;
  FGraphWidth:LongWord;
  FGraphHeight:LongWord;
  
  FLastX:LongWord;
  FLastY:LongWord;
  FOffsetX:LongWord;
  
 public
  property GraphWidth:LongWord read FGraphWidth;
  property GraphHeight:LongWord read FGraphHeight;
  
  procedure DrawBackground;
  procedure DrawGraph;
 
  procedure DrawData(AValue:LongWord);
  
 end;
 
 
type
 {And a TThread based thread class to continuously update our graph with new
  data from the sensor thread}
 TGraphThread = class(TThread)
  constructor Create(AData:TSensorData);
  
 private
  FData:TSensorData;
  FWindow:TGraphWindow;
  
 protected
  procedure Execute; override;
  
 end;
 
 
procedure StartGraphThread;
  
  
implementation


{Our graph page class}
constructor TGraphPage.Create(AData:TSensorData);
begin
 inherited Create;
 {Set the name or this HTTP Document (eg http://<Server>/graph)}
 Name:='/graph';
 FData:=AData;
 
end;


procedure TGraphPage.AddHeader(AResponse:THTTPServerResponse);
begin
 {Setup the response version and status}
 AResponse.Version:=HTTP_VERSION;
 AResponse.Status:=HTTP_STATUS_OK;
 AResponse.Reason:=HTTP_REASON_200;

 {Add the HTML content for the header of our graph page}
 AddContent(AResponse,'<!doctype html>');
 AddContent(AResponse,'<html>');
 AddContent(AResponse,'');
 AddContent(AResponse,'<head>');
 AddContent(AResponse,'    <title>Ultibo Sensormatic 3000</title>');
 AddContent(AResponse,'    <script src="/Chart.bundle.js"></script>');
 AddContent(AResponse,'    <script src="/jquery.min.js"></script>');
 AddContent(AResponse,'    <style>');
 AddContent(AResponse,'    canvas{');
 AddContent(AResponse,'        -moz-user-select: none;');
 AddContent(AResponse,'        -webkit-user-select: none;');
 AddContent(AResponse,'        -ms-user-select: none;');
 AddContent(AResponse,'    }');
 AddContent(AResponse,'    </style>');
 AddContent(AResponse,'</head>');
 AddContent(AResponse,'');
 AddContent(AResponse,'<body>');
 AddContent(AResponse,'    <div style="width:25%;">');
 AddContent(AResponse,'');
 AddContent(AResponse,'    </div>');
 AddContent(AResponse,'    <div style="width:50%;">');
 AddContent(AResponse,'        <canvas id="canvas"></canvas>');
 AddContent(AResponse,'    </div>');
 AddContent(AResponse,'    <br>');
 AddContent(AResponse,'    <br>');
 AddContent(AResponse,'    <script>');
 AddContent(AResponse,'        var data = {');
 AddContent(AResponse,'            datasets: [');
 AddContent(AResponse,'                {');
 AddContent(AResponse,'                    label: "Ultibo Sensormatic 3000",');
 AddContent(AResponse,'                    fill: false,');
 AddContent(AResponse,'                    lineTension: 0,');
 AddContent(AResponse,'                    backgroundColor: "rgba(255,0,0,0.4)",');
 AddContent(AResponse,'                    borderColor: "rgba(255,0,0,1)",');
 AddContent(AResponse,'                    borderCapStyle: ''butt'',');
 AddContent(AResponse,'                    borderDash: [],');
 AddContent(AResponse,'                    borderDashOffset: 0.0,');
 AddContent(AResponse,'                    borderJoinStyle: ''miter'',');
 AddContent(AResponse,'                    pointBorderColor: "rgba(255,0,0,1)",');
 AddContent(AResponse,'                    pointBackgroundColor: "#fff",');
 AddContent(AResponse,'                    pointBorderWidth: 1,');
 AddContent(AResponse,'                    pointHoverRadius: 5,');
 AddContent(AResponse,'                    pointHoverBackgroundColor: "rgba(255,0,0,1)",');
 AddContent(AResponse,'                    pointHoverBorderColor: "rgba(220,220,220,1)",');
 AddContent(AResponse,'                    pointHoverBorderWidth: 2,');
 AddContent(AResponse,'                    pointRadius: 1,');
 AddContent(AResponse,'                    pointHitRadius: 10,');
 AddContent(AResponse,'                    data: [');
 
end;


procedure TGraphPage.AddFooter(AResponse:THTTPServerResponse);
begin
 {Add the footer HTML content, you could do things much smarter
  than this but I'm sure you can see the potential}
 AddContent(AResponse,'                    ],');                
 AddContent(AResponse,'                    spanGaps: false,');
 AddContent(AResponse,'                }');
 AddContent(AResponse,'            ]');
 AddContent(AResponse,'        };');   
 AddContent(AResponse,'');       
 AddContent(AResponse,'        var options = {');
 AddContent(AResponse,'            showLines: true,');
 AddContent(AResponse,'            spanGaps: false,');
 AddContent(AResponse,'            scales: {');
 AddContent(AResponse,'                yAxes: [{');
 AddContent(AResponse,'                    type: ''linear'',');
 AddContent(AResponse,'                    ticks: {');
 AddContent(AResponse,'                        max: ' + IntToStr(FData.MaxY + 1) + ',');
 AddContent(AResponse,'                        min: 0,');
 AddContent(AResponse,'                        stepSize: 100');
 AddContent(AResponse,'                    }');
 AddContent(AResponse,'                }],');
 AddContent(AResponse,'                xAxes: [{');
 AddContent(AResponse,'                    type: ''linear'',');
 AddContent(AResponse,'                    position: ''bottom'',');
 AddContent(AResponse,'                    ticks: {');
 AddContent(AResponse,'                        max: ' + IntToStr(FData.MaxX + 1) + ',');
 AddContent(AResponse,'                        min: 0,');
 AddContent(AResponse,'                        stepSize: 100,');
 AddContent(AResponse,'                    }');
 AddContent(AResponse,'                }]');
 AddContent(AResponse,'            }');      
 AddContent(AResponse,'        };'); 
 AddContent(AResponse,'');  
 AddContent(AResponse,'        window.onload = function() {');
 AddContent(AResponse,'            var ctx = document.getElementById("canvas").getContext("2d");');
 AddContent(AResponse,'            var myLineChart = new Chart(ctx, {');
 AddContent(AResponse,'                type: ''line'',');
 AddContent(AResponse,'                data: data,');
 AddContent(AResponse,'                options: options');
 AddContent(AResponse,'            });');            
 AddContent(AResponse,'        };');
 AddContent(AResponse,'    </script>');
 AddContent(AResponse,'</body>');
 AddContent(AResponse,'');
 AddContent(AResponse,'</html>');
 
end;


procedure TGraphPage.AddContent(AResponse:THTTPServerResponse;const AContent:String);
begin
 {Add to the content of our page, when DoGet returns successfully everything in ContentString
  will be sent to the client that made the request}
 AResponse.ContentString:=AResponse.ContentString + AContent + HTTP_LINE_END;
 
end;


function TGraphPage.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
var
 Count:Integer;
 Total:Integer;
 Readings:PByte;
begin
 {The overridden DoGet method for our HTTP Document. The web server (THTTPListener) will call this method when
  a request is received which matches our registered URL, all we need to do is supply the content and a result}
 Result:=True;
 
 AddHeader(AResponse);
 
 Readings:=FData.GetReadings(Total);
 try
  for Count:=0 to Total do
   begin
    if Count < Total then
     begin
      AddContent(AResponse,'                           {x: ' + IntToStr(Count) + ',y: ' + IntToStr(Readings[Count]) + '},');
     end
    else
     begin
      AddContent(AResponse,'                           {x: ' + IntToStr(Count) + ',y: ' + IntToStr(Readings[Count]) + '}');
     end;
   end;
 finally
  FreeMem(Readings);
 end; 
 
 AddFooter(AResponse);
 
end;


{This is our graph window class}
constructor TGraphWindow.Create;
begin
 inherited Create;
 
 {Find the 8x8 font to use for the graph labels}
 FFont:=FontFindByName('Latin1-8x8');
 
 {Create a graphics window on the default console device}
 FHandle:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMRIGHT);
 if FHandle <> INVALID_HANDLE_VALUE then
  begin
   {Get the width and height so we can scale our graph}
   FWidth:=GraphicsWindowGetWidth(FHandle);
   FHeight:=GraphicsWindowGetHeight(FHandle);
   
   {Calculate the size available for display}
   {First the width}
   FGraphWidth:=100;
   while FGraphWidth < (FWidth - 100) do
    begin
     Inc(FGraphWidth,100);
    end;
   Dec(FGraphWidth,100);
   
   {Then the height}
   FGraphHeight:=100;
   while FGraphHeight < (FHeight - 100) do
    begin
     Inc(FGraphHeight,100);
    end;
   Dec(FGraphHeight,100);
   
   {Center up the graph in the window}
   FGraphLeft:=(FWidth - FGraphWidth) div 2;
   FGraphTop:=(FHeight - FGraphHeight) div 2;
   
   {Setup our starting point}
   FLastX:=1;
   FLastY:=0;
   FOffsetX:=24;
  end; 
  
end;


destructor TGraphWindow.Destroy; 
begin
 {Destroy our graphics window}
 GraphicsWindowDestroy(FHandle);
 
 inherited Destroy;
 
end;


procedure TGraphWindow.DrawBackground;
var
 Title:String;
 Count:LongWord;
 Rect:TConsoleRect;
begin
 {Draw the background of our graph, the title and labels plus the outline box}
 if FHandle = INVALID_HANDLE_VALUE then Exit;
 
 {Get the rectangle of our console graphics window}
 Rect:=GraphicsWindowGetRect(FHandle);
 
 {Clear the entire window to white}
 GraphicsWindowClearEx(FHandle,Rect.X1,Rect.Y1,Rect.X2,Rect.Y2,COLOR_WHITE);
 
 {Draw our title, centered at the top of the window}
 Title:='Ultibo Sensormatic 3000';
 GraphicsWindowDrawTextEx(FHandle,FFont,Title,((Rect.X2 - Rect.X1) - (FontGetWidth(FFont) * Length(Title))) div 2,((FGraphTop - Rect.Y1) - FontGetHeight(FFont)) div 2,COLOR_RED,COLOR_WHITE);
 
 {Draw a box around the actual graph in black}
 GraphicsWindowDrawBox(FHandle,FGraphLeft,FGraphTop,FGraphLeft + FGraphWidth - 1,FGraphTop + FGraphHeight - 1,COLOR_BLACK,1);
 
 {Put labels on both the X and Y axis}
 Count:=0;
 while Count <= FGraphWidth do
  begin
   GraphicsWindowDrawTextEx(FHandle,FFont,IntToStr(Count),FGraphLeft + Count - ((Length(IntToStr(Count)) * FontGetWidth(FFont)) div 2),FGraphTop + FGraphHeight + FontGetHeight(FFont) + 1,COLOR_GRAY,COLOR_WHITE);
   
   Inc(Count,100);
  end;
  
 Count:=0;
 while Count <= FGraphHeight do
  begin
   GraphicsWindowDrawTextEx(FHandle,FFont,IntToStr(Count),FGraphLeft - 1 - ((Length(IntToStr(Count)) + 1) * FontGetWidth(FFont)),(FGraphTop + FGraphHeight) - Count,COLOR_GRAY,COLOR_WHITE);
   
   Inc(Count,100);
  end;
 
end;


procedure TGraphWindow.DrawGraph;
var
 Count:LongWord;
begin
 {Draw the horizontal and vertical gridlines for the graph itself}
 if FHandle = INVALID_HANDLE_VALUE then Exit;
 
 {First the vertical, every 25 pixels}
 Count:=25;
 while Count < FGraphWidth do
  begin
   GraphicsWindowDrawLine(FHandle,FGraphLeft + Count,FGraphTop + 1,FGraphLeft + Count,FGraphTop + FGraphHeight - 2,COLOR_MIDGRAY,1);
   
   Inc(Count,25);
  end;
  
 {And then the horizontal}
 Count:=25;
 while Count < FGraphHeight do
  begin
   GraphicsWindowDrawLine(FHandle,FGraphLeft + 1,FGraphTop + Count,FGraphLeft + FGraphWidth - 2,FGraphTop + Count,COLOR_MIDGRAY,1);
   
   Inc(Count,25);
  end;
  
end;


procedure TGraphWindow.DrawData(AValue:LongWord);
var
 Count:LongWord;
 Source:TConsolePoint;
 Dest:TConsolePoint;
begin
 {Plot the actual data for our line graph, the value passed is the next value to draw}
 if FHandle = INVALID_HANDLE_VALUE then Exit;
 
 {Output the value to the log, just to show how. You could also output to a file or to the network etc}
 LoggingOutput('Sensormatic: Next graph value is ' + IntToStr(AValue));
 
 {Clamp our value within the allowed range}
 AValue:=Min(AValue,FGraphHeight);
 if AValue = 0 then AValue:=FLastY;
 
 {Check if we are going up or down}
 if AValue = FLastY then
  begin
   {Going nowhere, draw a single pixel only}
   GraphicsWindowDrawPixel(FHandle, FGraphLeft + FLastX, FGraphTop + FGraphHeight - FLastY, COLOR_RED);
  end
 else if AValue > FLastY then
  begin
   {Going up, draw a vertical line from new down to old}
   GraphicsWindowDrawLine(FHandle, FGraphLeft + FLastX, FGraphTop + FGraphHeight - AValue, FGraphLeft + FLastX, FGraphTop + FGraphHeight - FLastY, COLOR_RED, 1);
  end
 else if AValue < FLastY then 
  begin
   {Going down, draw a vertical line from old down to new}
   GraphicsWindowDrawLine(FHandle, FGraphLeft + FLastX, FGraphTop + FGraphHeight - FLastY, FGraphLeft + FLastX, FGraphTop + FGraphHeight - AValue, COLOR_RED, 1);
  end;
 
 {Save the value for next time}
 FLastY:=AValue;

 {Move to a our X and check if we reached the end}
 Inc(FLastX);
 if FLastX >= (FGraphWidth - 2) then
  begin
   {If we did, scroll the entire window left 1 pixel}
   FLastX:=FGraphWidth - 3;
   
   {Setup the source and destination points}
   Source.X:=FGraphLeft + 2;
   Source.Y:=FGraphTop + 1;
   Dest.X:=FGraphLeft + 1;
   Dest.Y:=FGraphTop + 1;
   
   {Copy the rectangle from source to destination}
   GraphicsWindowCopyImage(FHandle, Source, Dest, FGraphWidth - 3, FGraphHeight - 1);
   
   {Update our offset}
   Inc(FOffsetX);
   if FOffsetX >= 25 then
    begin
     FOffsetX:=0;

     {Offset of 25 or more, draw a new vertical line for the graph background}
     GraphicsWindowDrawLine(FHandle,FGraphLeft + FGraphWidth - 2,FGraphTop + 1,FGraphLeft + FGraphWidth - 2,FGraphTop + FGraphHeight - 2,COLOR_MIDGRAY,1);
    end
   else
    begin
     {Offset of less then 25, fill the last line with white}
     GraphicsWindowDrawLine(FHandle,FGraphLeft + FGraphWidth - 2,FGraphTop + 1,FGraphLeft + FGraphWidth - 2,FGraphTop + FGraphHeight - 2,COLOR_WHITE,1);
     
     {And then extend our horizontal graph lines into the last line}
     Count:=25;
     while Count < FGraphHeight do
      begin
       GraphicsWindowDrawPixel(FHandle,FGraphLeft + FGraphWidth - 2,FGraphTop + Count,COLOR_MIDGRAY);
       
       Inc(Count,25);
      end;
     
    end;    
  end; 
  
end;


{Our graph drawing thread}
constructor TGraphThread.Create(AData:TSensorData);
begin
 FData:=AData;
 inherited Create(False,THREAD_STACK_DEFAULT_SIZE);
 
end;


procedure TGraphThread.Execute; 
begin
 {The execute method, set the thread to Free on Terminate}
 FreeOnTerminate:=True;
 
 {And set the thread name}
 ThreadSetName(ThreadGetCurrent,'Graph Thread');

 {Create an instance of the graph window class}
 FWindow:=TGraphWindow.Create;
 FWindow.DrawBackground;
 FWindow.DrawGraph;
 
 {Check the sensor data min and max against the graph window width and height}
 if FData.MaxX > FWindow.GraphWidth - 1 then FData.MaxX:=FWindow.GraphWidth - 1;
 if FData.MaxY <> FWindow.GraphHeight - 1 then FData.MaxY:=FWindow.GraphHeight - 1;
 
 {Go into an endless loop reading and writing the values}
 while not Terminated do
  begin
   if FData <> nil then
    begin
     {Get the next value from the sensor data}
     {And write it to the graph window}
     FWindow.DrawData(FData.GetReading);
    end; 
   
   {Sleep for a second and do it again}
   Sleep(1000);
  end;  
 
 {If we even do exit, free the graph window object} 
 FWindow.Free; 
 
end;


{The wrapper function called from the main unit to start the graph}
procedure StartGraphThread;
begin
 {Create an instance of the graph thread and pass it the global sensor data object}
 TGraphThread.Create(SensorData);
 
end;


end.

