unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ValEdit, CheckLst, p600emuclasses, ueled, uEKnob,
  LazLogger, windows, strutils, typinfo, Midi, c7Seg, syncobjs, Types;

const
    WM_MIDI = WM_USER + 42;

type

  { TMainForm }

  TMainForm = class(TForm)
    bt0: TToggleBox;
    bt1: TToggleBox;
    bt10: TToggleBox;
    bt11: TToggleBox;
    bt12: TToggleBox;
    bt13: TToggleBox;
    bt14: TToggleBox;
    bt15: TToggleBox;
    btAutoCor: TToggleBox;
    btInit: TButton;
    btMetro: TToggleBox;
    btChorus: TToggleBox;
    btStart: TToggleBox;
    btTick: TButton;
    btSave: TButton;
    btUD: TToggleBox;
    btAppend: TToggleBox;
    btAssign: TToggleBox;
    btRecord: TToggleBox;
    btStacks: TToggleBox;
    btAD: TToggleBox;
    bt2: TToggleBox;
    bt3: TToggleBox;
    bt4: TToggleBox;
    bt5: TToggleBox;
    bt6: TToggleBox;
    bt7: TToggleBox;
    bt8: TToggleBox;
    bt9: TToggleBox;
    btFromTape: TToggleBox;
    btMidiCh: TToggleBox;
    btPrgRec: TToggleBox;
    btMidiMode: TToggleBox;
    btPrmEd: TToggleBox;
    btSeq: TToggleBox;
    btToTape: TToggleBox;
    kMasterTune: TuEKnob;
    kPrmVal: TuEKnob;
    kMod: TuEKnob;
    kTrkVol: TuEKnob;
    kSpeed: TuEKnob;
    kPitch: TuEKnob;
    l0: TuELED;
    l1: TuELED;
    lbxOutputDevices: TCheckListBox;
    lUD: TuELED;
    lAppend: TuELED;
    lAssign: TuELED;
    lRecord: TuELED;
    lV1: TuELED;
    lV2: TuELED;
    lV3: TuELED;
    lV4: TuELED;
    lV5: TuELED;
    lV6: TuELED;
    lStacks: TuELED;
    lSA: TuELED;
    lSB: TuELED;
    lSC: TuELED;
    lSD: TuELED;
    lPrgRec: TuELED;
    lPrmEd: TuELED;
    lA: TuELED;
    l2: TuELED;
    l3: TuELED;
    l4: TuELED;
    l5: TuELED;
    l6: TuELED;
    l7: TuELED;
    l8: TuELED;
    l9: TuELED;
    lB: TuELED;
    lC: TuELED;
    lD: TuELED;
    lChorus: TuELED;
    llCurCtrl: TLabel;
    lbxInputDevices: TCheckListBox;
    lvCV: TListView;
    lvGates: TListView;
    pnP600: TPanel;
    sA: TShape;
    sA1: TShape;
    sB: TShape;
    sB1: TShape;
    sC: TShape;
    sC1: TShape;
    sD: TShape;
    sD1: TShape;
    sDot: TShape;
    sDot1: TShape;
    sE: TShape;
    sE1: TShape;
    sF: TShape;
    sF1: TShape;
    sG: TShape;
    sG1: TShape;
    ssLeft: TSevenSegFrame;
    ssRight: TSevenSegFrame;
    tiTick: TTimer;
    tbRun: TToggleBox;
    procedure btInitClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btTickClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure kPitchMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure kPitchMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure kPrmValMouseEnter(Sender: TObject);
    procedure kPrmValMouseLeave(Sender: TObject);
    procedure lbxInputDevicesClickCheck(Sender: TObject);
    procedure lbxOutputDevicesClickCheck(Sender: TObject);
    procedure lvCVData(Sender: TObject; Item: TListItem);
    procedure lvGatesData(Sender: TObject; Item: TListItem);
    procedure tbRunChange(Sender: TObject);
    procedure tiTickTimer(Sender: TObject);
  private
    FMIDIXmitStream: TMemoryStream;
    FMIDIXmitRunningStatus: Byte;
    FMIDIXmitCount: Integer;

    { private declarations }
    procedure UpdateState;
    procedure DoMidiInData(const aDeviceIndex: integer; const aStatus, aData1, aData2: byte);
    procedure DoSysExData(const aDeviceIndex: integer; const aStream: TMemoryStream);
    procedure OnMidi(var AMsg:TMessage);message WM_MIDI;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

const
  CTimesRealSpeed=1;
  CStatusToByteCount : array[0..$f] of Integer = (-1, -1, -1, -1, -1, -1, -1, -1, 3, 3, 3, 3, 2, 2, 3, 1);

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  P600Emu := TProphet600Emulator.Create;
  FMIDIXmitStream := TMemoryStream.Create;

  Constraints.MinHeight:=Height;
  Constraints.MinWidth:=Width;

  lbxInputDevices.Items.Assign( MidiInput.Devices );
  lbxOutputDevices.Items.Assign( MidiOutput.Devices );
  MidiInput.OnMidiData := @DoMidiInData;
  MidiInput.OnSysExData := @DoSysExData;

  btInit.Click;

  lvCV.Items.Count:=Ord(High(TP600CV))+1;
  lvGates.Items.Count:=Ord(High(TP600Gate))+1;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  P600Emu.Free;
  FMIDIXmitStream.Free;

  MidiInput.CloseAll;
end;

procedure TMainForm.kPitchMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  kPrmValMouseEnter(Sender);
end;

procedure TMainForm.kPitchMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  kPrmValMouseEnter(Sender);
end;

procedure TMainForm.kPrmValMouseEnter(Sender: TObject);
var k:TuEKnob;
begin
  k:=Sender as TuEKnob;

  llCurCtrl.Caption:=Copy(k.Name,2,MaxInt)+' (Value = '+IntToStr(round(k.Position))+')';
end;

procedure TMainForm.kPrmValMouseLeave(Sender: TObject);
begin
  llCurCtrl.Caption:='';
end;

procedure TMainForm.lbxInputDevicesClickCheck(Sender: TObject);
begin
  if lbxInputDevices.Checked[ lbxInputDevices.ItemIndex ] then
    MidiInput.Open( lbxInputDevices.ItemIndex )
  else
    MidiInput.Close( lbxInputDevices.ItemIndex )
end;

procedure TMainForm.lbxOutputDevicesClickCheck(Sender: TObject);
begin
  if lbxOutputDevices.Checked[ lbxOutputDevices.ItemIndex ] then
    MidiOutput.Open( lbxOutputDevices.ItemIndex )
  else
    MidiOutput.Close( lbxOutputDevices.ItemIndex )
end;

procedure TMainForm.lvCVData(Sender: TObject; Item: TListItem);
const
  CIndexToCV : array[0..6*8 - 1] of TP600CV = (
    pcOsc1,pcOsc2,pcOsc3,pcOsc4,pcOsc5,pcOsc6,
    pcFil1,pcFil2,pcFil3,pcFil4,pcFil5,pcFil6,
    pcAmp1,pcAmp2,pcAmp3,pcAmp4,pcAmp5,pcAmp6,
    pcRes1,pcRes2,pcRes3,pcRes4,pcRes5,pcRes6,
    pcMix1,pcMix2,pcMix3,pcMix4,pcMix5,pcMix6,
    pcMod1,pcMod2,pcMod3,pcMod4,pcMod5,pcMod6,
    pcPW1,pcPW2,pcPW3,pcPW4,pcPW5,pcPW6,
    pcShape1,pcShape2,pcShape3,pcShape4,pcShape5,pcShape6
  );
var
  cv:TP600CV;
begin
  cv:=CIndexToCV[Item.Index];

  with P600Emu.HW do
  begin
    Item.Caption:=Copy(GetEnumName(TypeInfo(TP600CV),Ord(cv)),3,MaxInt);
    Item.SubItems.Add(IntToStr(CVValues[cv]));
    Item.SubItems.Add(FormatFloat('0.000',CVVolts[cv]));
    Item.SubItems.Add(FormatFloat('0.0',CVHertz[cv]));
  end;
end;

procedure TMainForm.lvGatesData(Sender: TObject; Item: TListItem);
var gt:TP600Gate;
begin
  gt:=TP600Gate(Item.Index);

  with P600Emu.HW do
  begin
    Item.Caption:=Copy(GetEnumName(TypeInfo(TP600Gate),Ord(gt)),3,MaxInt);
    Item.SubItems.Add(IntToStr(Ord(GateValues[gt])));
  end;
end;

procedure TMainForm.tbRunChange(Sender: TObject);
begin
  //HACK: open MIDI Yoke 2 on my PC...
  if (lbxInputDevices.Count >= 2) and AnsiContainsText(lbxInputDevices.Items[1], 'Yoke') then
  begin
    lbxInputDevices.Checked[1] := True;
    MidiInput.Open(1);
  end;
  if (lbxOutputDevices.Count >= 2) and AnsiContainsText(lbxOutputDevices.Items[1], 'Yoke') then
  begin
    lbxOutputDevices.Checked[1] := True;
    MidiOutput.Open(1);
  end;

  tiTick.Enabled:=tbRun.Checked;
end;

procedure TMainForm.tiTickTimer(Sender: TObject);
var i:Integer;
    sts, b, t0, t1, t2 : Byte;
begin
  for i:=0 to tiTick.Interval div cTickMilliseconds * CTimesRealSpeed - 1 do
    P600Emu.Tick;

  while P600Emu.HW.RecvMIDIByte(b) do
  begin
    if FMIDIXmitStream.Size = 0 then
    begin
      if (b and $80) <> 0 then // status?
      begin
        sts := b;
        if (sts and $f0) <> $f0 then // not realtime?
          FMIDIXmitRunningStatus := sts;
      end
      else
      begin
        sts := FMIDIXmitRunningStatus;
        FMIDIXmitStream.WriteByte(sts);
      end;

      FMIDIXmitCount := CStatusToByteCount[sts shr 4];

      if sts = $f0 then // sysex start?
        FMIDIXmitCount := MaxInt;
    end;

    FMIDIXmitStream.WriteByte(b);

    if (FMIDIXmitStream.Size >= FMIDIXmitCount) or (b = $f7) then
    begin
      FMIDIXmitStream.Position := 0;

      if b = $f7 then
      begin
        for i := 0 to lbxOutputDevices.Count - 1 do
          if lbxOutputDevices.Checked[i] then
            MidiOutput.SendSysEx(i, FMIDIXmitStream)
      end
      else
      begin
        t0 := 0;
        if FMIDIXmitStream.Size >= 1 then
          t0 := FMIDIXmitStream.ReadByte;

        t1 := 0;
        if FMIDIXmitStream.Size >= 2 then
          t1 := FMIDIXmitStream.ReadByte;

        t2 := 0;
        if FMIDIXmitStream.Size >= 3 then
          t2 := FMIDIXmitStream.ReadByte;

        for i := 0 to lbxOutputDevices.Count - 1 do
          if lbxOutputDevices.Checked[i] then
          begin
            MidiOutput.Send(i, t0, t1, t2);
            DebugLn(Format( '%s: <Status> %.2x, <Data 1> %.2x <Data 2> %.2x', [ MidiOutput.Devices[i], t0, t1, t2 ] ));
          end;
      end;

      FMIDIXmitStream.Clear;
    end;
  end;

  UpdateState;
end;

procedure TMainForm.btTickClick(Sender: TObject);
begin
  P600Emu.Tick;
  UpdateState;
end;

procedure TMainForm.btInitClick(Sender: TObject);
begin
  DebugLn(Caption);
  P600Emu.Initialize;
end;

procedure TMainForm.btSaveClick(Sender: TObject);
begin
  P600Emu.SaveRamToFile;
end;

procedure TMainForm.UpdateState;

  function PotToValue(APot: Double): Integer;
  begin
    Result := round(APot * 5.0 / 4.0 * 256.0);
  end;

var cv:TP600CV;
    gt:TP600Gate;
    c: TToggleBox;
    i: Integer;
begin
  with P600Emu.HW do
  begin
    l0	.Active := pl0	        in LEDStates;
    l1	.Active := pl1	        in LEDStates;
    l2	.Active := pl2	        in LEDStates;
    l3	.Active := pl3	        in LEDStates;
    l4	.Active := pl4	        in LEDStates;
    l5	.Active := pl5	        in LEDStates;
    l6	.Active := pl6	        in LEDStates;
    l7	.Active := pl7	        in LEDStates;
    l8	.Active := pl8	        in LEDStates;
    l9	.Active := pl9	        in LEDStates;
    lA	.Active := plA	        in LEDStates;
    lB	.Active := plB	        in LEDStates;
    lC	.Active := plC	        in LEDStates;
    lD	.Active := plD	        in LEDStates;
    lPrmEd	.Active := plPrmEd	in LEDStates;
    lPrgRec	.Active := plPrgRec	in LEDStates;
    lV1	.Active := plT1	        in LEDStates;
    lV2	.Active := plT2	        in LEDStates;
    lV3	.Active := plT3	        in LEDStates;
    lV4	.Active := plT4	        in LEDStates;
    lV5	.Active := plT5	        in LEDStates;
    lV6	.Active := plT6	        in LEDStates;
    lStacks	.Active := plStack	in LEDStates;
    lRecord	.Active := plRecord	in LEDStates;
    lSA	.Active := plSA	        in LEDStates;
    lSB	.Active := plSB	        in LEDStates;
    lSC	.Active := plSC	        in LEDStates;
    lSD	.Active := plSD	        in LEDStates;
    lUD	.Active := plUD	        in LEDStates;
    lAssign	.Active := plAssign	in LEDStates;
    lAppend	.Active := plAppend	in LEDStates;
    lChorus	.Active := plChorus	in LEDStates;

    ssLeft.Value:=SevenSegment[0];
    ssRight.Value:=SevenSegment[1];

    PotValues[ppTune]:=PotToValue(kMasterTune.Position);
    PotValues[ppValue]:=PotToValue(kPrmVal.Position);
    PotValues[ppSpeed]:=PotToValue(kSpeed.Position);
    PotValues[ppTrkVol]:=PotToValue(kTrkVol.Position);
    PotValues[ppMod]:=PotToValue(kMod.Position);
    PotValues[ppPitch]:=PotToValue(kPitch.Position);

    lvCV.Invalidate;
    lvGates.Invalidate;

    for i := 0 to ComponentCount - 1 do
    begin
      if not (Components[i] is TToggleBox) then
        Continue;
      c := Components[i] as TToggleBox;
      if c.Tag < 0 then
        Continue;
      P600Emu.HW.ButtonStates[TP600Button(c.Tag)] := c.Checked;
    end;
  end;
end;

procedure TMainForm.DoMidiInData(const aDeviceIndex: integer; const aStatus,
  aData1, aData2: byte);
begin
  // MIDI classes don't seem to be thread safe, posting a message shoud be ok tho
  PostMessage(Handle,WM_MIDI,aDeviceIndex,aData1 or (aData2 shl 8) or (aStatus shl 16));
end;

procedure TMainForm.DoSysExData(const aDeviceIndex: integer; const aStream: TMemoryStream);
begin
  while aStream.Position < aStream.Size do
    P600Emu.HW.SendMIDIByte(aStream.ReadByte);
end;

procedure TMainForm.OnMidi(var AMsg: TMessage);
var
  devIndex:Integer;
  status,data1,data2:Byte;
begin
  devIndex:=AMsg.wParam;
  status:=AMsg.lParam shr 16;
  data2:=AMsg.lParam shr 8;
  data1:=AMsg.lParam;

  // print the message log
  DebugLn(Format( '%s: <Status> %.2x, <Data 1> %.2x <Data 2> %.2x', [ MidiInput.Devices[devIndex], status, data1, data2 ] ));

  if CStatusToByteCount[status shr 4] >= 1 then
    P600Emu.HW.SendMIDIByte(status);

  if CStatusToByteCount[status shr 4] >= 2 then
    P600Emu.HW.SendMIDIByte(data1);

  if CStatusToByteCount[status shr 4] >= 3 then
    P600Emu.HW.SendMIDIByte(data2);

  AMsg.Result:=0;
end;

end.

