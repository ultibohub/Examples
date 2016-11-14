program LogOutput;

{$mode objfpc}{$H+}

{ Example 09 Log Output                                                        }
{                                                                              }
{  A simple example of how to output logging information to a console window.  }
{                                                                              }
{                                                                              }
{  Ultibo supports logging to the console, a file or to the network.           }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{                                                                              }
{  QEMU VersatilePB version                                                    }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  QEMUVersatilePB,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  SysUtils,
  GlobalConfig, {Include the global configuration unit so we can modify some parameters}
  Logging;      {Include the logging unit}

{A window handle and a counter.}
var
 Count:Integer;
 WindowHandle:TWindowHandle;


begin
 {Create our window on the left side this time to allow room for logging}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);

 {Output the message}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 09 Log Output');
 ConsoleWindowWriteLn(WindowHandle,'');

 {Because console logging is disabled by default we need to enable it first.

  This can also be done using the command line parameter CONSOLE_REGISTER_LOGGING=1
  in the cmdline.txt file on the SD card.

  }
 CONSOLE_REGISTER_LOGGING:=True;

 {Now we can register a new console logging device by passing the default console
 device to the function LoggingConsoleDeviceAdd().

 The parameters CONSOLE_LOGGING_DEFAULT and CONSOLE_LOGGING_POSITION control if
 the new logging device can be the default device and where the console window
 will appear. By default the logging console will be on the right side of the
 screen.}
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);

 {We can set the new console logging device as the default by using the function
  LoggingDeviceSetDefault() and passing to it the first available console logging
  device obtained by LoggingDeviceFindByType()}
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));

 {The console logging window should now be visible on the right side of the screen
  and we can try writing to it using LoggingOutput().}
 ConsoleWindowWriteLn(WindowHandle,'Sending a message to the log');
 LoggingOutput('This is a logging message, it should go to the logging screen');

 {Sleep a few seconds so we can see that}
 Sleep(3000);

 {Notice that the logging output automatically includes a counter and the ThreadId
  of the thread that sent the message, this helps with debugging your program.

  On the Raspberry Pi 2 the logging message will also include the CPU Id of the CPU
  that the thread was running on when it sent the message.

  You might also notice a logging message saying that a logging device was created
  and enabled. Many parts of Ultibo automatically send logging messages to indicate
  events and errors.}

 {Let's send a few more messages to the log}
 ConsoleWindowWriteLn(WindowHandle,'Sending lots of messages to the log');
 for Count:=1 to 100 do
  begin
   LoggingOutput('Message no ' + IntToStr(Count) + ' sent at ' + DateTimeToStr(Now));

   {Sleep for a random amount of time to mix up the output}
   Sleep(Random(350));
  end;

 {The logging output is handled by its own thread so even in this simple example
  logging is handled in the background by the logging unit, your code just sends
  the message and doesn't have to think about where the logs are going after that.}

 {Halt the thread}
 ThreadHalt(0);
end.

