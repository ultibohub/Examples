program HelloTiger;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Tiger                                           }
{                                                                              }
{ A basic OpenVG example showing a rotating picture of a tiger, this example   }
{ uses a picture embedded in the code.                                         }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+.      }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 WindowHandle:TWindowHandle;

{Link our C library to include the original example} 
{$linklib hello_tiger}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_tiger; cdecl; external 'hello_tiger' name 'hello_tiger';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Tiger');
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_tiger;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Tiger');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

