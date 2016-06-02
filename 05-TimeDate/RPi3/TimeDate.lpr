program TimeDate;

{$mode objfpc}{$H+}

{ Example 05 Time Date                                                         }
{                                                                              }
{  The Raspberry Pi has no real time clock built in to the board so the normal }
{  way to get the correct time is from an internet time server.                }
{                                                                              }
{  In this example the network will be started and an IP address obtained from }
{  DHCP, once that has happened the time will be obtained via NTP.             }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 3B version                                                     }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2837,
  BCM2710,
  SysUtils,
  DateUtils,
  Ultibo,   {The Ultibo unit provides some APIs for getting and setting timezones}
  Services, {The services unit includes the NTP client and will automatically include the network}
  DWCOTG,   {We need to include the USB host driver for the Raspberry Pi}
  SMSC95XX; {We also need to include the driver for the Raspberry Pi network adapter}

{A window handle so we can see what is happening.}
var
 Counter:LongWord;
 WindowHandle:TWindowHandle;


begin
 {Create a console window at full size}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Output out welcome text on the console window}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 05 Date Time');
 ConsoleWindowWriteLn(WindowHandle,'Waiting for network address to be configured and NTP time synchronisation to occur');

 {Print out the current time and date}
 {This will probably show 30-12-99 which is midnight on 30 December 1899, this is the default value of Free Pascal time}
 ConsoleWindowWriteLn(WindowHandle,'The current date and time is ' + DateTimeToStr(Now));

 {Initialize a variable so we can count how long we've been waiting}
 Counter:=0;

 {Let's wait for a while for the time to be updated}
 while YearOf(Now) < 2000 do
  begin
   {Sleep for a second}
   Sleep(1000);

   {Update our counter}
   Inc(Counter);

   {Check how long we have waited}
   if Counter > 90 then
    begin
     {Print a failure message on the console}
     ConsoleWindowWriteLn(WindowHandle,'Sorry, failed to get the time after 90 seconds. Is the network connected?');

     {Break out of the loop and continue}
     Break;
    end;
  end;

 {Check out counter to see if we got here by success of failure}
 if Counter <= 90 then
  begin
   {We must have been successful so let's print the date and time again}
   ConsoleWindowWriteLn(WindowHandle,'Success, time has been updated from the internet');
   ConsoleWindowWriteLn(WindowHandle,'The date and time is now ' + DateTimeToStr(Now));

   {How about showing the date and time in some different ways?}
   {Time only}
   ConsoleWindowWriteLn(WindowHandle,'Time is ' + TimeToStr(Time));
   {Date only}
   ConsoleWindowWriteLn(WindowHandle,'Date is ' + DateToStr(Date));
   {Date and Time formatted to the long format}
   ConsoleWindowWriteLn(WindowHandle,'Formatted Date and Time is ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now));
   ConsoleWindowWriteLn(WindowHandle,'');

   {Unless you live in certain parts of the world then the dates and times shown might not seem correct}
   {Let's print the timezone to see what it is set to}
   ConsoleWindowWriteLn(WindowHandle,'Timezone is ' + GetCurrentTimezone);

   {Now we can set the timezone to another place and see what the time is there}
   {Australia}
   ConsoleWindowWriteLn(WindowHandle,'Setting Timezone to "AUS Eastern Standard Time"');
   SetCurrentTimezone('AUS Eastern Standard Time');
   ConsoleWindowWriteLn(WindowHandle,'The date and time is now ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now));
   ConsoleWindowWriteLn(WindowHandle,'');

   {North America}
   ConsoleWindowWriteLn(WindowHandle,'Setting Timezone to "Central Standard Time"');
   SetCurrentTimezone('Central Standard Time');
   ConsoleWindowWriteLn(WindowHandle,'The date and time is now ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now));
   ConsoleWindowWriteLn(WindowHandle,'');

   {India}
   ConsoleWindowWriteLn(WindowHandle,'Setting Timezone to "India Standard Time"');
   SetCurrentTimezone('India Standard Time');
   ConsoleWindowWriteLn(WindowHandle,'The date and time is now ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now));
   ConsoleWindowWriteLn(WindowHandle,'');

   {Western Europe}
   ConsoleWindowWriteLn(WindowHandle,'Setting Timezone to "W. Europe Standard Time"');
   SetCurrentTimezone('W. Europe Standard Time');
   ConsoleWindowWriteLn(WindowHandle,'The date and time is now ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now));
   ConsoleWindowWriteLn(WindowHandle,'');

   {You can set the default timezone on start by setting the TIMEZONE_DEFAULT_NAME parameter in cmdline.txt
    e.g. TIMEZONE_DEFAULT_NAME=Central_Standard_Time}
   
   {That's it for example 05, try out example 06 next}
  end;

 {Halt this thread, the example is done}
 ThreadHalt(0);
end.

