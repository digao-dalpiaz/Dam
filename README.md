
# Dam

## Delphi Message Dialogs with Formatted Text

![Delphi Supported Versions](https://img.shields.io/badge/Delphi%20Supported%20Versions-XE2..10.3%20Rio-blue.svg)
![Delphi Supported Versions](https://img.shields.io/badge/Dam%20Version-3.0-green.svg)

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

The Message Dialog: :stuck_out_tongue:

![Dam Message Dialog](Dam_Dialog.png)

## How to install

1. First you need to install my HTLabel component. Get it here: https://github.com/digao-dalpiaz/HTLabel

   *This is a label with HTML formatting component. The messages uses this component to display formatted text.*

2. Open the package DamPackage in the Delphi. Then Build and Install.

3. Add "Lib" sub folder to Delphi Library Path.

4. Run AfterBuild.bat to publish DFM and RES to Lib folder.

> Supports Delphi XE2..Delphi 10.3 Rio

# Supported Languages

At this time, the following languages are available for messages:

- English
- Portuguese
- Spanish
- German
- Italian
- Chinese
- Japanese
- Greek
- Russian
- French

The language set the caption of buttons in the message dialogs, like OK, Cancel, Yes, No, and the title of the form, when Information, Question, Warning, Error and Message (custom). The form title can be changed in the message properties and can be automatically set by other objects, like parent form title, main form title and application title.

> You can open an issue to send me a new language translation or correct current translation

# How to use

The main ideia is to drop one TDam in the main form, or main data module. The TDam is a container that have all TDamMsg (message dialog).

To manage messages, just double-click at the TDam component.

**But how to call the messages?**

The TDam has a property called `DamUnitName`. Here you define a unit name to contain all message methods. When you use the function **Build Unit**, the component will create this unit automatically at project folder with the methods based in all created messages (with the same name as the message name).

So, at any unit, you just add a uses to this unit, and call the message you want by message name. If the message has parameters, the method will be declared automatically asking for parameters in an array.

**Yeah, but if I'am at the same unit that is the TDam and TDamMsg messages, if I call method by message name, the Delphi think I'm reffering the object of message!**

This is true, and because of that, the component has a object hidden function. You only need to name the message starting with `_`. So the unit builder will remove this character to name the method and the Delphi will not confuse the two things.

I recomend you to create all messages named starting with the `_` character.

If you use the wizard, the name will be automatically start with this character.

*There is a button on the Dam manager to hide/un-hide all selected messages.*

> Remember, every time you change the messages, you need to rebuld the unit. Just click the button Build Unit to update.

**OK, but I don't want to use this unit! Is there another method?**

Yes, of course. You can call the message by using the functions of TDamMsg object. Just use the message object `Run` or `RunAsBool` functions. In this case I recomend you to not hide the message, so you can call by the object name as is.

> But I honestly recommend you to use the unit as it will make much more sense. :wink:

