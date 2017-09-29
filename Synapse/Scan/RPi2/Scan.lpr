program Scan;

{$mode delphi}{$H+}

{ Synapse example - Ping Scan                                                  }
{                                                                              }
{ A multithreaded ping scan example                                            }
{                                                                              }
{ Change the values of ScanStart and ScanEnd below to set the start and end    }
{ of the range to be scanned.                                                  }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{                                                                              }
{ This example was adapted for Ultibo from the Synapse scan example.           }
{                                                                              }
{ Note: If you downloaded this example separate from the Synapse zip file      }
{ then you may need to add the path to the Synapse sources to your project     }
{ options.                                                                     }
{                                                                              }
{ To do that go to Project, Project Options... in the Lazarus menu and then    }
{ select Compiler Options, Other unit files (-Fu) and enter the correct path.  }
{                                                                              }

uses
  RaspberryPi2,
  GlobalConst,
  Threads,
  Classes,
  SysUtils,
  Console,
  IPUtils,
  PingThread;

var
 Handle:THandle;
 ScanStart:String = '192.168.0.1'; {Change these addresses to suit your network}
 ScanEnd:String = '192.168.0.254';
 
 i,j:Cardinal;
 PingStart:Cardinal;
 PingEnd:Cardinal;
 PingCount:Cardinal;
 PingResults:array of TPingResult;
 PingThreads:array of TPingThread;
 ThreadsComplete:Boolean;

begin
 {Create our console window}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
  
 ConsoleWindowWriteLn(Handle,'Starting Synapse Ping Scan Example');
  
 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
 ConsoleWindowWriteLn(Handle,'');
 
 {Wait a few seconds for the network to be ready}
 Sleep(3000);
 
 {Check start and end}
 if IsIPAddress(ScanStart) and IsIPAddress(ScanEnd) then
  begin
   PingStart:=IPToCardinal(StrToIP(ScanStart));
   PingEnd:=IPToCardinal(StrToIP(ScanEnd));
   
   {Display addresses}
   ConsoleWindowWriteLn(Handle,'Start address  ' + ScanStart);
   ConsoleWindowWriteLn(Handle,'End address  ' + ScanEnd);
   ConsoleWindowWriteLn(Handle,'');
   
   {Get count}
   PingCount:=(PingEnd - PingStart) + 1;

   {Display count}
   ConsoleWindowWrite(Handle,'Pinging ' + IntToStr(PingCount) + ' addresses');
   
   {Initialize arrays}
   SetLength(PingResults,PingCount);
   SetLength(PingThreads,PingCount);
   j:=0;
   for i:=PingStart to PingEnd do
    begin
     PingResults[j].IPAddress:=IPToStr(CardinalToIP(i));
     PingResults[j].Exists:=False;
     Inc(j);
    end;

   {Create one thread for each ping}
   for i:=0 to PingCount - 1 do
    begin
     PingThreads[i]:=TPingThread.Create(PingResults[i]);
    end;

   ConsoleWindowWrite(Handle,' ');

   {Wait till all threads are executed}
   repeat
    ThreadsComplete:=True;
    ConsoleWindowWrite(Handle,'.');
    Sleep(1000);
    for i:=0 to PingCount - 1 do
     begin
      if not PingThreads[i].Ready then
       begin
        ThreadsComplete:=False;
        Break;
       end;
     end;
   until ThreadsComplete;

   ConsoleWindowWriteLn(Handle,'');
   ConsoleWindowWriteLn(Handle,'');
   ConsoleWindowWriteLn(Handle,'Ping Results');

   {Dislay results}
   for i:=0 to PingCount - 1 do
    begin
     if PingThreads[i].PingResult.Exists then
      begin
       ConsoleWindowWriteLn(Handle,IntToStr(i + 1) + '  ' + PingThreads[i].PingResult.IPAddress);
      end;
    end;

   {Free threads}
   for i:=0 to PingCount - 1 do
    begin
     PingThreads[i].Free;
    end;
  end
 else
  begin
   ConsoleWindowWriteLn(Handle,'Invalid start or end address for scan');
  end;  

 {Halt the main thread} 
 ThreadHalt(0);
end.
 