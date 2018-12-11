program SerialEcho;

{$mode delphi}{$H+}

{ Synapse example - Serial Echo                                                }
{                                                                              }
{ An example that reads characters from the serial port and echos them back.   }
{                                                                              }
{ You will need a serial cable or a USB to serial converter to connect the Pi  }
{ to your computer, the Pi uses pin 14 (Transmit) and pin 15 (Receive) as well }
{ as a Ground pin to make the connection. The documentation shows you where to }
{ find each of the pins on the Raspberry Pi.                                   }
{                                                                              }
{ Raspberry Pi Model A and B (26 pin header)                                   }
{   https://www.raspberrypi.org/documentation/usage/gpio/                      }
{                                                                              }
{ Raspberry Pi Models A+/B+/Zero/2B/3B/3B+/3A+ (40 pin header)                 }
{   https://www.raspberrypi.org/documentation/usage/gpio-plus-and-raspi2/      }
{                                                                              }
{ You will also need a terminal program running on your computer, you can use  }
{ something like PuTTY to create a serial connection to the COM port you are   }
{ using. For this example we'll use these connection settings:                 }
{                                                                              }
{ Speed: 9600                                                                  }
{ Data Bits: 8                                                                 }
{ Stop Bits: 1                                                                 }
{ Parity: None                                                                 }
{ Flow Control: None                                                           }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+.      }
{                                                                              }
{ This example was adapted for Ultibo from the Synapse modem example.          }
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
  SynaSer;

var
 Handle:THandle;
 Character:Char;
 Characters:String;
 BlockSerial:TBlockSerial;

begin
 try
  {Create our console window}
  Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
   
  ConsoleWindowWriteLn(Handle,'Starting Synapse Serial Echo Example');
   
  {Wait a couple of seconds for C:\ drive to be ready}
  ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
  while not DirectoryExists('C:\') do
   begin
    {Sleep for a second}
    Sleep(1000);
   end;
  ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
  ConsoleWindowWriteLn(Handle,'');
  
  {Create blocking serial object}
  BlockSerial:=TBlockSerial.Create;
  BlockSerial.RaiseExcept:=True;
  try
   {Connect}
   BlockSerial.Connect('Serial0');
   if BlockSerial.LastError = 0 then
    begin
     ConsoleWindowWriteLn(Handle,'Connected');
     
     {Configure}
     BlockSerial.Config(9600,8,'N',0,False,False);
     if BlockSerial.LastError = 0 then
      begin
       ConsoleWindowWriteLn(Handle,'Configured');
       
       {Loop endlessly waiting for data}
       while True do
        begin
         {Receive a string of characters (Up to the CR LF terminator)}
         //Characters:=BlockSerial.RecvString(-1);                               //The RecvString method only uses CR LF
         Characters:=BlockSerial.RecvTerminated(-1,Chr(13)); //Chr(13) + Chr(10) //Change these values if your terminal program uses something different
         ConsoleWindowWriteLn(Handle,'Received string: ' + Characters);
          
         if Uppercase(Characters) = 'QUIT' then
          begin
           {If received then say goodbye and exit our loop}
           Characters:='Goodbye!' + Chr(13) + Chr(10);
           BlockSerial.SendString(Characters);
   
           {Wait for the data to be sent}
           Sleep(1000);
   
           Break;
          end;
          
         {Add a carriage return and line feed}
         Characters:=Characters + Chr(13) + Chr(10);
 
         {And echo them back to the serial device}
         BlockSerial.SendString(Characters);
        end; 
      end
     else
      begin
       ConsoleWindowWriteLn(Handle,'Failed to configure serial port');
      end;
    end
   else
    begin
     ConsoleWindowWriteLn(Handle,'Failed to connect to serial port');
    end;
  finally
   BlockSerial.free;
  end;
  
  {Halt the main thread} 
  ThreadHalt(0);
 except
  on E: Exception do
   begin
    ConsoleWindowWriteLn(Handle,'An exception happened at address ' + IntToHex(PtrUInt(ExceptAddr),8) + ' the message was ' + E.Message);
   end;
 end; 
end.
 