unit SensorUnit;

{$mode delphi}{$H+}

{ The sensor unit to read the important data from our Sensormatic 3000 and     }
{ store it ready for use by the other threads                                  }

interface

uses
  GlobalConfig,
  GlobalConst,
  Platform,
  Threads,
  Font,
  Console,
  SysUtils,
  Classes,
  Syncobjs;

  
type
 {A class for storing the sensor data}
 TSensorData = class(TObject)
  constructor Create;
  destructor Destroy; override;
 private
  FLock:TCriticalSection;
  FData:PByte;
  
  FMaxX:Integer;
  FMaxY:Integer;
  
  FNextGet:Integer;
  FNextPut:Integer;
 public
  property MaxX:Integer read FMaxX write FMaxX;
  property MaxY:Integer read FMaxY write FMaxY;
  
  function GetReading:Byte;
  procedure PutReading(AReading:Byte);
  
  function GetReadings(var ATotal:Integer):PByte;
 end;
 
 
type
 {A thread class for "reading" the sensor, just a standard TThread object
  with the Execute method overridden to implement the functionality we want}
 TSensorThread = class(TThread)
  constructor Create(AData:TSensorData);
 private
  FData:TSensorData;
  
 protected
  procedure Execute; override;
 end;

 
var
 SensorData:TSensorData;

 
procedure StartSensorThread;

  
implementation


uses MainUnit;


{Our sensor data class}
constructor TSensorData.Create;
begin
 inherited Create;

 {We use a TCriticalSection object directly from the RTL for locking}
 FLock:=TCriticalSection.Create;
 
 {Allocate some memory for our data}
 FData:=AllocMem(10000);
 
 {Setup some defaults}
 FMaxX:=9999;
 FMaxY:=255;
 
 FNextGet:=0;
 FNextPut:=0;
end;


destructor TSensorData.Destroy; 
begin
 {Free the allocated memory}
 FreeMem(FData);
 
 {Destroy our critical section}
 FLock.Free;
 
 inherited Destroy;
 
end;


function TSensorData.GetReading:Byte;
begin
 {Acquire the lock}
 FLock.Acquire;
 
 {Get the next reading}
 Result:=FData[FNextGet mod FMaxX];
 
 {Increment the next counter}
 Inc(FNextGet);
 if FNextGet > FNextPut then FNextGet:=FNextPut;
 
 {Release the lock}
 FLock.Release;
 
end;


procedure TSensorData.PutReading(AReading:Byte);
begin
 {Lock}
 FLock.Acquire;
 
 {Store the data in the next slot}
 FData[FNextPut mod FMaxX]:=Min(AReading,FMaxY);
 
 {And increment next}
 Inc(FNextPut);
 
 {Unlock}
 FLock.Release;
 
end;


function TSensorData.GetReadings(var ATotal:Integer):PByte;
var
 Count:Integer;
begin
 {Lock before we start}
 FLock.Acquire;

 {Allocate some memory}
 Result:=GetMem(FMaxX + 1);
 
 {Calculate homw many readings there are}
 ATotal:=Min(Max(FNextPut,1) - 1,FMaxX);
 
 {Copy each reading}
 for Count:=0 to ATotal do
  begin
   if FNextPut < FMaxX then
    begin
     Result[Count]:=FData[Count]; 
    end
   else
    begin
     Result[Count]:=FData[((FNextPut - FMaxX) + Count) mod FMaxX];
    end;    
  end;
 
 {Unlock when we are finished}
 FLock.Release;
 
end;


{The sensor reading thread}
constructor TSensorThread.Create(AData:TSensorData);
begin
 {Store the data object}
 FData:=AData;
 
 {Call the inherited method}
 inherited Create(False,THREAD_STACK_DEFAULT_SIZE);
 
end;


procedure TSensorThread.Execute; 
var
 Last:Integer;
 Value:Integer;
begin
 {Free this thread when it terminates}
 FreeOnTerminate:=True;
 
 {Set the name of the thread}
 ThreadSetName(ThreadGetCurrent,'Sensormatic Thread');
 
 {Create our sensor window to log the readings}
 SensorWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,False);
 
 {Setup the window with our defaults}
 ConsoleWindowSetForecolor(SensorWindow,COLOR_DARKGREEN);
 ConsoleWindowSetBackcolor(SensorWindow,COLOR_MIDGRAY);
 ConsoleWindowSetFont(SensorWindow,FontFindByName('Latin1-8x14'));
 ConsoleWindowSetViewport(SensorWindow,1,1,ConsoleWindowGetWidth(SensorWindow),ConsoleWindowGetHeight(SensorWindow));
 
 {Write the starting message}
 ConsoleWindowWriteLn(SensorWindow,'Sensormatic thread starting at ' + DateTimeToStr(Now));
  
 {Randomize the number generator for our sensor "readings"}
 Randomize;
 RandSeed:=ClockGetTime; //To Do //Note that there is currently a bug in the RTL which "initializes" the random seed with the same value every time
 
 {Get an initial value}
 Last:=Random(250);
 
 {Loop reading our sensor}
 while not Terminated do
  begin
   Value:=Random(250);
   if Value > Last then
    begin
     {Up}
     Value:=Min(Last + Random(19),250)
    end
   else if Value < Last then 
    begin
     {Down}
     Value:=Max(Last - Random(19),0)
    end;
   Last:=Value; 
    
   {Print the latest reading on the console} 
   ConsoleWindowWriteLn(SensorWindow,'Sensormatic reading at ' + DateTimeToStr(Now) + ' is ' + IntToStr(Value));
   
   {Store the reading in our data class}
   FData.PutReading(Value);
   
   {Sleep a while and start again}
   Sleep(1000);
  end;  
 
end;


{A wrapper to create our thread and sensor data}
procedure StartSensorThread;
begin
 {Create an instance of the sensor data class}
 SensorData:=TSensorData.Create;
 
 {And an instance of the sensor thread}
 TSensorThread.Create(SensorData);
 
end;


end.

