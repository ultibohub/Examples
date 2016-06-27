program RAMDisk;

{$mode objfpc}{$H+}

{ Advanced example - RAM Disk                                                  }
{                                                                              }
{ In this example we look at the advanced functions of the file system to do   }
{ tasks like creating a virtual disk image, partitioning a disk and formatting }
{ a disk.                                                                      }
{                                                                              }
{ All of the file system API works in much the same way so formatting a real   }
{ disk is no different to formatting a virtual disk image.                     }
{                                                                              }
{ Using this example as a guide you can work out how to do many of these tasks }
{ but be aware formatting is NOT reversible so if you destroy all the data on  }
{ your hard disk don't say we didn't warn you.                                 }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{ To create a version for the A/B/A+/B+/Zero simply create a new project and   }
{ copy this code into it.                                                      }

uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  BCM2836,
  BCM2709,
  SysUtils,
  Ultibo,          {Include the Ultibo unit for some data types}
  FileSystem,      {Include the file system unit}
  FATFS,           {And the FAT file system driver}
  NTFS,            {Plus the NTFS file system driver}
  VirtualDisk,     {Also include the virtual disk unit to allow RAM and other disk types}
  ConsoleShell,    {Add the console shell and the filesystem commands so we can try out}
  ShellFilesystem, {the results. We also need the keyboard and USB driver for that.}
  Keyboard,
  DWCOTG;

{We need a window handle plus some file system variables}
var
 Handle:TWindowHandle;

 ImageNo:Integer;
 Device:TDiskDevice;
 Partition:TDiskPartition;
 Volume:TDiskVolume;
 Drive:TDiskDrive;

begin
  {Create our window}
  Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);

  {Output the message}
  ConsoleWindowWriteLn(Handle,'Welcome to the RAM Disk advanced example');
  ConsoleWindowWriteLn(Handle,'');

  {Wait a couple of seconds for C:\ drive to be ready}
  ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
  while not DirectoryExists('C:\') do
   begin
    {Sleep for a second}
    Sleep(1000);
   end;
  ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
  ConsoleWindowWriteLn(Handle,'');

  ConsoleWindowWriteLn(Handle,'Creating 10MB RAM disk with 512 byte sectors');

  {Create a RAM Disk virtual disk image, this requires a few parameters to set the
   type of disk plus the size and some other information.

   The FileSystem unit creates an instance of the TFileSysDriver object which is
   always available to access advanced functions that are not provided by the normal
   file API. All functions of the file system are accessible using the FileSysDriver
   object, it will determine the correct sub system to handle our request based on
   the parameters we pass.

   To create any sort of disk image we need to call FileSysDriver.CreateImage()

   The first parameter allows passing an existing image, we can ignore that in this case.
   The second parameter is a name which for a RAM disk can be anything you choose.

   Then we need to pass an image type, these are defined in the FileSystem unit,
   for our RAM disk we need to pass itMEMORY to create a memory image.

   The next two parameters are the type of media, we'll use mtREMOVABLE this time to
   create a removable disk and we can pass ftUNKNOWN for the FloppyType parameter
   because we are not creating a floppy disk image.

   After that comes the disk attributes, we want this to be a disk and to be both
   readable and writeable.

   Then we need some information about the geometry of the disk, we pass 512 for the
   sector size which is pretty standard and then 20480 for the sector count which will
   give us a 10MB RAM disk (20480 x 512 = 10485760 bytes)

   We can simply ignore the cylinders, heads and sectors values and the partition id
   because they are not needed for this example.

   The CreateImage function returns an image number which we can then pass to other
   functions to manage our new disk image}
  ImageNo:=FileSysDriver.CreateImage(0,'RAM Disk',itMEMORY,mtREMOVABLE,ftUNKNOWN,iaDisk or iaReadable or iaWriteable,512,20480,0,0,0,pidUnused);
  if ImageNo <> 0 then
   begin
    {If the returned image number is not zero then create image was successful}
    ConsoleWindowWriteLn(Handle,'Successfully created RAM disk image');

    {Now we need to mount the image so we can work with it as a disk.

     We simply pass the returned image number to MountImage}
    if FileSysDriver.MountImage(ImageNo) then
     begin
      ConsoleWindowWriteLn(Handle,'Successfully mounted RAM disk image');

      {To use our RAM disk it needs to be formatted just like any other disk.

       We can't format an image directly so we need to obtain the disk volume that
       represents our image. First we get the disk device from the image number}
      Device:=FileSysDriver.GetDeviceByImage(FileSysDriver.GetImageByNo(ImageNo,False,FILESYS_LOCK_NONE),False,FILESYS_LOCK_NONE);
      if Device <> nil then
       begin
        ConsoleWindowWriteLn(Handle,'Located RAM disk image as disk device ' + Device.Name);

        {Now we can get the disk volume from our device}
        Volume:=FileSysDriver.GetVolumeByDevice(Device,False,FILESYS_LOCK_NONE);
        if Volume <> nil then
         begin
          ConsoleWindowWriteLn(Handle,'Located RAM disk image as disk volume ' + Volume.Name);

          {Finally we can format the volume using the FormatVolume method.

           Format volume needs to know the name of the volume to format which is
           simply Volume.Name and what sort of floppy disk it is formatting, since
           our RAM disk is not a floppy at all we pass the value ftUNKNOWN.

           It also needs the file system type, we'll format our RAM disk as FAT so
           we pass fsFAT12, this doesn't mean the disk will be formatted as FAT12
           because format volume will decide the correct FAT type based on the size
           of the volume we are formatting.

           Note: You can pass either fsFAT16 or fsFAT32 to specify the exact type
           to use but the format may fail if the type is incompatible with the size}
          if FileSysDriver.FormatVolume(Volume.Name,ftUNKNOWN,fsFAT12) then
           begin
            ConsoleWindowWriteLn(Handle,'Formatted RAM disk image as FAT');

            {To get access to our RAM disk using the file API we need to know
             what drive letter was assigned to it, we can get the disk drive
             object by calling GetDriveByVolume}
            Drive:=FileSysDriver.GetDriveByVolume(Volume,False,FILESYS_LOCK_NONE);
            if Drive <> nil then
             begin
              ConsoleWindowWriteLn(Handle,'RAM disk image mounted as drive ' + Drive.Name);

              {At this point we can use any of the standard file API functions to
               read and write files and manipulate the contents of our RAM disk}

             end;
           end;
         end;
       end;
     end;
   end;
  ConsoleWindowWriteLn(Handle,'');


  {Let's try a slightly more complex example, this time we'll create 100MB RAM disk
   but we'll make it a fixed disk instead of a removable and partition and format
   the disk using the NTFS file system}
  ConsoleWindowWriteLn(Handle,'Creating 100MB NTFS RAM disk with 512 byte sectors');

  {We use the create image method again, this time we pass the value mtFIXED for the
   MediaType parameter and 204800 for the sector count (204800 x 512 = 104857600 bytes).

   You can also use create image to create a file based virtual disk by passing the
   value itFILE for the ImageType parameter. In that case the name would be a file
   name to use for the virtual disk image}
  ImageNo:=FileSysDriver.CreateImage(0,'NTFS RAM Disk',itMEMORY,mtFIXED,ftUNKNOWN,iaDisk or iaReadable or iaWriteable,512,204800,0,0,0,pidUnused);
  if ImageNo <> 0 then
   begin
    {Again check for image number not equal to zero for success}
    ConsoleWindowWriteLn(Handle,'Successfully created NTFS RAM disk image');

    {And mount the disk image by passing the image number to mount image}
    if FileSysDriver.MountImage(ImageNo) then
     begin
      ConsoleWindowWriteLn(Handle,'Successfully mounted NTFS RAM disk image');

      {Because this is a fixed disk we need to create a partition on it before we
       can go ahead and format the disk.

       We still need to obtain the disk device first by calling GetDeviceByImage}
      Device:=FileSysDriver.GetDeviceByImage(FileSysDriver.GetImageByNo(ImageNo,False,FILESYS_LOCK_NONE),False,FILESYS_LOCK_NONE);
      if Device <> nil then
       begin
        ConsoleWindowWriteLn(Handle,'Located NTFS RAM disk image as disk device ' + Device.Name);

        {Now we call CreatePartition passing the device, the type and the size.

         Because we want to format as NTFS we need to create an NTFS partition,
         these have a defined type of pidHPFSNTFS.

         The size can be any number of sectors up to the full size of the device
         less a small amount for overhead. The easiest way to create a partition
         using the complete disk is to use the value returned by Device.AvailableSectors.

         Create partition will handle all the details of aligning to cylinder boundaries
         and include appropriate offsets etc, so our partition will likely be slightly
         smaller than the total size of the disk.

         The final parameter indicates whether to make this the active partition or not}
        if FileSysDriver.CreatePartition(Device.Name,'',pidHPFSNTFS,Device.AvailableSectors,True) then
         begin
          ConsoleWindowWriteLn(Handle,'Created an NTFS partition');

          {To format the new partition we need to obtain the disk volume again,
           everything in the file system follows a hierarchy. A disk contains the
           partitions, the partitions contain the volumes and the volumes contain
           the drives.

           Get our new partition from the disk device}
          Partition:=FileSysDriver.GetPartitionByDevice(Device,False,FILESYS_LOCK_NONE);
          if Partition <> nil then
           begin
            ConsoleWindowWriteLn(Handle,'Located NTFS RAM disk image as disk partition ' + Partition.Name);

            {The final step is to get the disk volume from our partition}
            Volume:=FileSysDriver.GetVolumeByPartition(Partition,False,FILESYS_LOCK_NONE);
            if Volume <> nil then
             begin
              ConsoleWindowWriteLn(Handle,'Located NTFS RAM disk image as disk volume ' + Volume.Name);

              {We format the volume as NTFS this time, there are 3 versions of NTFS which
               are supported by the driver but it is probably best to stay with the latest
               version which is fsNTFS51}
              if FileSysDriver.FormatVolume(Volume.Name,ftUNKNOWN,fsNTFS51) then
               begin
                ConsoleWindowWriteLn(Handle,'Formatted NTFS RAM disk image as NTFS');

                {As before we can get the drive from the volume so we know what drive
                 letter to pass to the file system APIs}
                Drive:=FileSysDriver.GetDriveByVolume(Volume,False,FILESYS_LOCK_NONE);
                if Drive <> nil then
                 begin
                  ConsoleWindowWriteLn(Handle,'NTFS RAM disk image mounted as drive ' + Drive.Name);

                  {The NTFS RAM disk is now available for use}

                 end;
               end;
             end;
           end;
         end;
       end;
     end;
   end;
end.

