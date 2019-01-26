
# Dam

## Delphi Message Dialogs with Formatted Text

## Introduction

Almost every application you build, you need to add message dialogs.

Delphi provides you a few options, like ShowMessage, MessageDlg and MessageBox.

As your project gets bigger, the dialogs may mess up your code, mainly if you need to write many lines in the messages, with many parameters.

If you need to format some part of the message, you need to add labels in a custom form. This is could be really bad.

Figure out this message:

```delphi
begin
  ShowMessage('The task was scheduled to run at the time: '+aTime+'.'+#13#10+
    #13#10+
    'Please, check the schedule at main board.'+
    ' If you want to change schedule, find by ID #'+IntToStr(IDNumber)+'.'+#13#10+
    #13#10+
    'This task was created by user '+aUserName+'. Only this user can edit this task profile.');
end;
```

So, I think this is really ugly, don't you think? :persevere:

**And if I tell you this can be written much more beautiful:** :smile:

```delphi
begin
  InfoTaskScheduled([aTime, IDNumber, aUserName]);
end;
```

This component allows you to manage all your application messages in a "container". You can create de message dialog with a wizard creator, allowing you to format the message (bold, italic, underline, font name, font size, text background color, align center, align right, use tab alignment, clicable links, etc).

You can specify the message buttons, the icon, the form title, and much more.

Then you specify a name for the message. So, you can call the message at any part of your application, just calling the method by the message name!

If you specify parameters in the message text, then when you call the message method, you can specify parameters that will be replaced in the text.

*Please read carefully this documentation to know how this component works.* :grin:

## Screenshots

The Dam container listing all messages created, stored at DFM:

![Dam List](Dam_List.png)

The Message Wizard Creator, to customize the message dialog:

![DamMsg Editor](DamMsg_Editor.png)

## How to install

1. First you need to install my HTLabel component. Get it here: https://github.com/digao-dalpiaz/HTLabel

   This is a label with HTML formatting component. The messages uses this component to display formatted text.

2. Open the package DamPackage in the Delphi. Then Build and Install.

3. Add "Lib" sub folder to Delphi Library Path.

4. Run AfterBuild.bat to publish DFM and RES to Lib folder.

> Supports Delphi XE2..Delphi 10.3 Rio

