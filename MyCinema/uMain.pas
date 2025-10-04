unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TReservation = class
  public
    Id: string;
    MovieTitle: string;
    City: string;
    ShowDate: string;
    ShowTime: string;
    ProjectionType: string;
    SeatRow: Integer;
    SeatCol: Integer;
    PriceRSD: Integer;
    PaymentMethod: string;
  end;

  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    Page: TPageControl;
    TabLogin, TabHome, TabMovie, TabOptions, TabSeats, TabCheckout, TabMyRes, TabResDetail: TTabSheet;

    Reservations: TObjectList<TReservation>;
    CurrentMovie, CurrentCity, CurrentDate, CurrentTime, CurrentType: string;
    CurrentSeatRow, CurrentSeatCol: Integer;
    EditingReservation: TReservation;

    edtUser, edtPass: TEdit;
    btnLogin: TButton;
    lblLoginMsg: TLabel;

    btnWatch, btnMyReservations: TButton;

    btnMovie1, btnMovie2, btnToOptionsFromMovie: TButton;
    lblMovie: TLabel;

    grpCity, grpDate, grpTime, grpType: TGroupBox;
    cbKrag, cbBgd, cbD1, cbD2, cbT1, cbT2, cbP1, cbP2: TCheckBox;
    btnToSeats: TButton;

    pnlSeats: TPanel;
    btnReserve: TButton;
    lblSeatInfo: TLabel;
    SeatPanels: array[0..7,0..7] of TPanel;

    lblSummary: TLabel;
    grpPayment: TGroupBox;
    cbGPay, cbAPay, cbCard: TCheckBox;
    btnContinue: TButton;

    lstReservations: TListBox;
    btnOpenReservation: TButton;
    btnBackHomeFromMyRes: TButton;

    lblResDetail: TLabel;
    btnChangeSeat: TButton;
    btnCancelRes: TButton;
    btnBackToMyRes: TButton;

    UpdatingPayment: Boolean;

    procedure BuildUI;
    procedure GoToTab(ATab: TTabSheet);
    procedure OnlyOneChecked(Sender: TObject);
    procedure OnlyOnePaymentChecked(Sender: TObject);
    procedure LoadTakenSeatsFromReservations;
    procedure SeatClick(Sender: TObject);
    procedure RefreshSeatSelectionLabel;
    procedure RebuildReservationsList;
    function  CalcPrice(const AType, ATime: string): Integer;
    procedure SaveCurrentAsReservation;
    function  IsSameShow(const R: TReservation): Boolean;
    function  IsSeatAlreadyReserved(Row, Col: Integer; Exclude: TReservation): Boolean;

    procedure OnLogin(Sender: TObject);
    procedure OnWatchMovie(Sender: TObject);
    procedure OnGoMyReservations(Sender: TObject);
    procedure OnMoviePick(Sender: TObject);
    procedure OnToSeats(Sender: TObject);
    procedure OnReserveSeat(Sender: TObject);
    procedure OnContinue(Sender: TObject);
    procedure OnOpenReservation(Sender: TObject);
    procedure OnChangeSeat(Sender: TObject);
    procedure OnCancelRes(Sender: TObject);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := 'MyCinema';
  Width := 900;
  Height := 600;
  Position := poScreenCenter;

  Reservations := TObjectList<TReservation>.Create(True);
  BuildUI;
  GoToTab(TabLogin);
end;

procedure TFormMain.BuildUI;
var
  RowIdx, ColIdx: Integer;
  sp: TPanel;
begin
  Page := TPageControl.Create(Self);
  Page.Parent := Self;
  Page.Align := alClient;

  TabLogin := TTabSheet.Create(Page); TabLogin.PageControl := Page; TabLogin.Caption := 'Login';
  TabHome := TTabSheet.Create(Page); TabHome.PageControl := Page; TabHome.Caption := 'Home';
  TabMovie := TTabSheet.Create(Page); TabMovie.PageControl := Page; TabMovie.Caption := 'Film';
  TabOptions := TTabSheet.Create(Page); TabOptions.PageControl := Page; TabOptions.Caption := 'Opcije';
  TabSeats := TTabSheet.Create(Page); TabSeats.PageControl := Page; TabSeats.Caption := 'Sedišta';
  TabCheckout := TTabSheet.Create(Page); TabCheckout.PageControl := Page; TabCheckout.Caption := 'Plaćanje';
  TabMyRes := TTabSheet.Create(Page); TabMyRes.PageControl := Page; TabMyRes.Caption := 'Moje Rezervacije';
  TabResDetail := TTabSheet.Create(Page); TabResDetail.PageControl := Page; TabResDetail.Caption := 'Detalj';

  // LOGIN
  with TLabel.Create(TabLogin) do begin Parent := TabLogin; Caption := 'Пријава (username: admin, password: 1234)'; Left := 20; Top := 20; Font.Style := [fsBold]; end;
  with TLabel.Create(TabLogin) do begin Parent := TabLogin; Caption := 'Корисничко име:'; Left := 20; Top := 60; end;
  edtUser := TEdit.Create(TabLogin); edtUser.Parent := TabLogin; edtUser.Left := 160; edtUser.Top := 56; edtUser.Width := 200;
  with TLabel.Create(TabLogin) do begin Parent := TabLogin; Caption := 'Лозинка:'; Left := 20; Top := 100; end;
  edtPass := TEdit.Create(TabLogin); edtPass.Parent := TabLogin; edtPass.Left := 160; edtPass.Top := 96; edtPass.Width := 200; edtPass.PasswordChar := '*';
  btnLogin := TButton.Create(TabLogin); btnLogin.Parent := TabLogin; btnLogin.Caption := 'Улогуј се'; btnLogin.Left := 160; btnLogin.Top := 140; btnLogin.OnClick := OnLogin;
  lblLoginMsg := TLabel.Create(TabLogin); lblLoginMsg.Parent := TabLogin; lblLoginMsg.Left := 160; lblLoginMsg.Top := 180; lblLoginMsg.Font.Color := clRed;

  // HOME
  with TLabel.Create(TabHome) do begin Parent := TabHome; Caption := 'Добродошли!'; Left := 20; Top := 20; Font.Size := 12; Font.Style := [fsBold]; end;
  btnWatch := TButton.Create(TabHome); btnWatch.Parent := TabHome; btnWatch.Caption := 'Gledaj film'; btnWatch.Left := 20; btnWatch.Top := 70; btnWatch.Width := 200; btnWatch.OnClick := OnWatchMovie;
  btnMyReservations := TButton.Create(TabHome); btnMyReservations.Parent := TabHome; btnMyReservations.Caption := 'Moje rezervacije'; btnMyReservations.Left := 20; btnMyReservations.Top := 110; btnMyReservations.Width := 200; btnMyReservations.OnClick := OnGoMyReservations;

  // MOVIE
  lblMovie := TLabel.Create(TabMovie); lblMovie.Parent := TabMovie; lblMovie.Left := 20; lblMovie.Top := 20; lblMovie.Caption := 'Изаберите филм:'; lblMovie.Font.Style := [fsBold];
  btnMovie1 := TButton.Create(TabMovie); btnMovie1.Parent := TabMovie; btnMovie1.Caption := 'Film A'; btnMovie1.Left := 20; btnMovie1.Top := 60; btnMovie1.Width := 200; btnMovie1.OnClick := OnMoviePick;
  btnMovie2 := TButton.Create(TabMovie); btnMovie2.Parent := TabMovie; btnMovie2.Caption := 'Film B'; btnMovie2.Left := 240; btnMovie2.Top := 60; btnMovie2.Width := 200; btnMovie2.OnClick := OnMoviePick;
  btnToOptionsFromMovie := TButton.Create(TabMovie); btnToOptionsFromMovie.Parent := TabMovie; btnToOptionsFromMovie.Caption := 'Dalje'; btnToOptionsFromMovie.Left := 20; btnToOptionsFromMovie.Top := 110; btnToOptionsFromMovie.Enabled := False; btnToOptionsFromMovie.OnClick := OnToSeats;

  // OPTIONS
  grpCity := TGroupBox.Create(TabOptions); grpCity.Parent := TabOptions; grpCity.Caption := 'Град'; grpCity.Left := 20; grpCity.Top := 20; grpCity.Width := 200; grpCity.Height := 90;
  cbKrag := TCheckBox.Create(grpCity); cbKrag.Parent := grpCity; cbKrag.Caption := 'Kragujevac'; cbKrag.Left := 10; cbKrag.Top := 20; cbKrag.OnClick := OnlyOneChecked;
  cbBgd  := TCheckBox.Create(grpCity); cbBgd.Parent := grpCity;  cbBgd.Caption := 'Beograd';    cbBgd.Left := 10; cbBgd.Top := 45; cbBgd.OnClick := OnlyOneChecked;

  grpDate := TGroupBox.Create(TabOptions); grpDate.Parent := TabOptions; grpDate.Caption := 'Датум пројекције'; grpDate.Left := 240; grpDate.Top := 20; grpDate.Width := 220; grpDate.Height := 90;
  cbD1 := TCheckBox.Create(grpDate); cbD1.Parent := grpDate; cbD1.Caption := '10.12.2025'; cbD1.Left := 10; cbD1.Top := 20; cbD1.OnClick := OnlyOneChecked;
  cbD2 := TCheckBox.Create(grpDate); cbD2.Parent := grpDate; cbD2.Caption := '20.12.2025'; cbD2.Left := 10; cbD2.Top := 45; cbD2.OnClick := OnlyOneChecked;

  grpTime := TGroupBox.Create(TabOptions); grpTime.Parent := TabOptions; grpTime.Caption := 'Време'; grpTime.Left := 480; grpTime.Top := 20; grpTime.Width := 180; grpTime.Height := 90;
  cbT1 := TCheckBox.Create(grpTime); cbT1.Parent := grpTime; cbT1.Caption := '14:00'; cbT1.Left := 10; cbT1.Top := 20; cbT1.OnClick := OnlyOneChecked;
  cbT2 := TCheckBox.Create(grpTime); cbT2.Parent := grpTime; cbT2.Caption := '20:00'; cbT2.Left := 10; cbT2.Top := 45; cbT2.OnClick := OnlyOneChecked;

  grpType := TGroupBox.Create(TabOptions); grpType.Parent := TabOptions; grpType.Caption := 'Тип пројекције'; grpType.Left := 680; grpType.Top := 20; grpType.Width := 160; grpType.Height := 90;
  cbP1 := TCheckBox.Create(grpType); cbP1.Parent := grpType; cbP1.Caption := '2D'; cbP1.Left := 10; cbP1.Top := 20; cbP1.OnClick := OnlyOneChecked;
  cbP2 := TCheckBox.Create(grpType); cbP2.Parent := grpType; cbP2.Caption := '3D'; cbP2.Left := 10; cbP2.Top := 45; cbP2.OnClick := OnlyOneChecked;

  btnToSeats := TButton.Create(TabOptions); btnToSeats.Parent := TabOptions; btnToSeats.Caption := 'Na sedišta'; btnToSeats.Left := 20; btnToSeats.Top := 130; btnToSeats.OnClick := OnToSeats;

  // SEATS
  pnlSeats := TPanel.Create(TabSeats); pnlSeats.Parent := TabSeats; pnlSeats.Left := 20; pnlSeats.Top := 20; pnlSeats.Width := 8*44; pnlSeats.Height := 8*44; pnlSeats.BevelOuter := bvNone;
  for RowIdx := 0 to 7 do
    for ColIdx := 0 to 7 do begin
      sp := TPanel.Create(pnlSeats);
      sp.Parent := pnlSeats;
      sp.Tag := RowIdx*100 + ColIdx;
      sp.Left := ColIdx*44; sp.Top := RowIdx*44; sp.Width := 40; sp.Height := 40;
      sp.Caption := ''; sp.BevelOuter := bvLowered; sp.ParentBackground := False;
      sp.OnClick := SeatClick;
      SeatPanels[RowIdx,ColIdx] := sp;
    end;
  lblSeatInfo := TLabel.Create(TabSeats); lblSeatInfo.Parent := TabSeats; lblSeatInfo.Left := 20; lblSeatInfo.Top := pnlSeats.Top + pnlSeats.Height + 10; lblSeatInfo.Caption := 'Изаберите слободно седиште.';
  btnReserve := TButton.Create(TabSeats); btnReserve.Parent := TabSeats; btnReserve.Left := 20; btnReserve.Top := lblSeatInfo.Top + 30; btnReserve.Width := 120; btnReserve.Caption := 'Rezerviši'; btnReserve.Enabled := False; btnReserve.OnClick := OnReserveSeat;

  // CHECKOUT
  lblSummary := TLabel.Create(TabCheckout); lblSummary.Parent := TabCheckout;
  lblSummary.Left := 20; lblSummary.Top := 20; lblSummary.AutoSize := True; lblSummary.WordWrap := True; lblSummary.Width := 520; lblSummary.Caption := '';

  grpPayment := TGroupBox.Create(TabCheckout); grpPayment.Parent := TabCheckout; grpPayment.Caption := 'Начин плаћања'; grpPayment.Left := 20; grpPayment.Top := 60; grpPayment.Width := 250; grpPayment.Height := 110;
  cbGPay := TCheckBox.Create(grpPayment); cbGPay.Parent := grpPayment; cbGPay.Caption := 'Google Pay'; cbGPay.Left := 10; cbGPay.Top := 20; cbGPay.OnClick := OnlyOnePaymentChecked;
  cbAPay := TCheckBox.Create(grpPayment); cbAPay.Parent := grpPayment; cbAPay.Caption := 'Apple Pay'; cbAPay.Left := 10; cbAPay.Top := 45; cbAPay.OnClick := OnlyOnePaymentChecked;
  cbCard := TCheckBox.Create(grpPayment); cbCard.Parent := grpPayment; cbCard.Caption := 'Kartica'; cbCard.Left := 10; cbCard.Top := 70; cbCard.OnClick := OnlyOnePaymentChecked;

  btnContinue := TButton.Create(TabCheckout); btnContinue.Parent := TabCheckout; btnContinue.Caption := 'Nastavi'; btnContinue.Left := 20; btnContinue.Top := 190; btnContinue.OnClick := OnContinue;

  // MY RESERVATIONS
  lstReservations := TListBox.Create(TabMyRes); lstReservations.Parent := TabMyRes; lstReservations.Left := 20; lstReservations.Top := 20; lstReservations.Width := 500; lstReservations.Height := 350;
  btnOpenReservation := TButton.Create(TabMyRes); btnOpenReservation.Parent := TabMyRes; btnOpenReservation.Left := 20; btnOpenReservation.Top := 380; btnOpenReservation.Caption := 'Отвори'; btnOpenReservation.OnClick := OnOpenReservation;
  btnBackHomeFromMyRes := TButton.Create(TabMyRes); btnBackHomeFromMyRes.Parent := TabMyRes; btnBackHomeFromMyRes.Left := 110; btnBackHomeFromMyRes.Top := 380; btnBackHomeFromMyRes.Caption := 'Назад'; btnBackHomeFromMyRes.OnClick := OnGoMyReservations;

  // RESERVATION DETAIL
  lblResDetail := TLabel.Create(TabResDetail); lblResDetail.Parent := TabResDetail; lblResDetail.Left := 20; lblResDetail.Top := 20; lblResDetail.AutoSize := True; lblResDetail.WordWrap := True; lblResDetail.Width := 600; lblResDetail.Caption := '';
  btnChangeSeat := TButton.Create(TabResDetail); btnChangeSeat.Parent := TabResDetail; btnChangeSeat.Left := 20; btnChangeSeat.Top := 80; btnChangeSeat.Caption := 'Промени седиште'; btnChangeSeat.OnClick := OnChangeSeat;
  btnCancelRes := TButton.Create(TabResDetail); btnCancelRes.Parent := TabResDetail; btnCancelRes.Left := 160; btnCancelRes.Top := 80; btnCancelRes.Caption := 'Откажи'; btnCancelRes.OnClick := OnCancelRes;
  btnBackToMyRes := TButton.Create(TabResDetail); btnBackToMyRes.Parent := TabResDetail; btnBackToMyRes.Left := 240; btnBackToMyRes.Top := 80; btnBackToMyRes.Caption := 'Назад'; btnBackToMyRes.OnClick := OnGoMyReservations;
end;

procedure TFormMain.GoToTab(ATab: TTabSheet);
begin
  Page.ActivePage := ATab;
end;

procedure TFormMain.OnlyOneChecked(Sender: TObject);
var
  Cb: TCheckBox;
  ParentBox: TWinControl;
  I: Integer;
begin
  Cb := Sender as TCheckBox;
  if not Cb.Checked then Exit;

  ParentBox := Cb.Parent;
  for I := 0 to ParentBox.ControlCount - 1 do
    if (ParentBox.Controls[I] is TCheckBox) and (ParentBox.Controls[I] <> Cb) then
      TCheckBox(ParentBox.Controls[I]).Checked := False;
end;

procedure TFormMain.OnlyOnePaymentChecked(Sender: TObject);
begin
  if UpdatingPayment then Exit;   // anti-recursion guard
  UpdatingPayment := True;
  try
    cbGPay.OnClick := nil; cbAPay.OnClick := nil; cbCard.OnClick := nil;
    cbGPay.Checked := (Sender = cbGPay);
    cbAPay.Checked := (Sender = cbAPay);
    cbCard.Checked := (Sender = cbCard);
  finally
    cbGPay.OnClick := OnlyOnePaymentChecked;
    cbAPay.OnClick := OnlyOnePaymentChecked;
    cbCard.OnClick := OnlyOnePaymentChecked;
    UpdatingPayment := False;
  end;
end;

// Da li je rezervacija za isti termin/film/grad/tip kao trenutno izabrano
function TFormMain.IsSameShow(const R: TReservation): Boolean;
begin
  Result := (R.MovieTitle = CurrentMovie) and
            (R.City = CurrentCity) and
            (R.ShowDate = CurrentDate) and
            (R.ShowTime = CurrentTime) and
            (R.ProjectionType = CurrentType);
end;

// Provera duplog sedišta (uz opciono izuzimanje jedne rezervacije kada uređujemo)
function TFormMain.IsSeatAlreadyReserved(Row, Col: Integer; Exclude: TReservation): Boolean;
var
  I: Integer;
  Res: TReservation;
begin
  Result := False;
  for I := 0 to Reservations.Count-1 do
  begin
    Res := Reservations[I];
    if (Res <> Exclude) and IsSameShow(Res) and (Res.SeatRow = Row) and (Res.SeatCol = Col) then
      Exit(True);
  end;
end;

// Popuni mrežu zauzetim mestima na osnovu postojećih rezervacija
procedure TFormMain.LoadTakenSeatsFromReservations;
var
  RowIdx, ColIdx, I: Integer;
  Res: TReservation;
begin
  // reset mreže sedišta
  for RowIdx := 0 to 7 do
    for ColIdx := 0 to 7 do
    begin
      SeatPanels[RowIdx, ColIdx].Enabled := True;
      SeatPanels[RowIdx, ColIdx].Caption := '';
      SeatPanels[RowIdx, ColIdx].Color := clBtnFace;
      SeatPanels[RowIdx, ColIdx].Tag := RowIdx*100 + ColIdx;
    end;

  // označi zauzeta prema postojećim rezervacijama za ISTI film/termin
  for I := 0 to Reservations.Count-1 do
  begin
    Res := Reservations[I];
    if IsSameShow(Res) then
    begin
      // ako uređujemo baš ovu rezervaciju, ostavi njeno sedište slobodno
      if (EditingReservation <> nil) and (Res = EditingReservation) then
        Continue;

      if (Res.SeatRow in [0..7]) and (Res.SeatCol in [0..7]) then
      begin
        SeatPanels[Res.SeatRow, Res.SeatCol].Enabled := False;
        SeatPanels[Res.SeatRow, Res.SeatCol].Caption := 'X';
        SeatPanels[Res.SeatRow, Res.SeatCol].Color := clGray;
      end;
    end;
  end;

  CurrentSeatRow := -1;
  CurrentSeatCol := -1;
  btnReserve.Enabled := False;
  RefreshSeatSelectionLabel;
end;

procedure TFormMain.SeatClick(Sender: TObject);
var
  Pnl: TPanel;
  RowIdx, ColIdx: Integer;
begin
  Pnl := Sender as TPanel;
  if not Pnl.Enabled then Exit;

  if (CurrentSeatRow >= 0) and (CurrentSeatCol >= 0) then
    if SeatPanels[CurrentSeatRow, CurrentSeatCol].Enabled then
      SeatPanels[CurrentSeatRow, CurrentSeatCol].Color := clBtnFace;

  RowIdx := Pnl.Tag div 100;
  ColIdx := Pnl.Tag mod 100;
  CurrentSeatRow := RowIdx;
  CurrentSeatCol := ColIdx;

  Pnl.Color := clSkyBlue;

  btnReserve.Enabled := True;
  RefreshSeatSelectionLabel;
end;

procedure TFormMain.RefreshSeatSelectionLabel;
begin
  if (CurrentSeatRow >= 0) and (CurrentSeatCol >= 0) then
    lblSeatInfo.Caption := Format('Изабрано седиште: ред %d, колона %d',[CurrentSeatRow+1, CurrentSeatCol+1])
  else
    lblSeatInfo.Caption := 'Изаберите слободно седиште.';
end;

function TFormMain.CalcPrice(const AType, ATime: string): Integer;
begin
  if SameText(AType, '2D') then
    if ATime = '14:00' then Result := 450 else Result := 550
  else
    if ATime = '14:00' then Result := 650 else Result := 800;
end;

procedure TFormMain.SaveCurrentAsReservation;
var
  Res: TReservation;
begin
  if Assigned(EditingReservation) then
  begin
    EditingReservation.MovieTitle := CurrentMovie;
    EditingReservation.City := CurrentCity;
    EditingReservation.ShowDate := CurrentDate;
    EditingReservation.ShowTime := CurrentTime;
    EditingReservation.ProjectionType := CurrentType;
    EditingReservation.SeatRow := CurrentSeatRow;
    EditingReservation.SeatCol := CurrentSeatCol;
    EditingReservation.PriceRSD := CalcPrice(CurrentType, CurrentTime);
    if cbGPay.Checked then EditingReservation.PaymentMethod := 'Google Pay'
    else if cbAPay.Checked then EditingReservation.PaymentMethod := 'Apple Pay'
    else EditingReservation.PaymentMethod := 'Kartica';
  end
  else
  begin
    Res := TReservation.Create;
    Res.Id := FormatDateTime('yyyymmddhhnnsszzz', Now);
    Res.MovieTitle := CurrentMovie;
    Res.City := CurrentCity;
    Res.ShowDate := CurrentDate;
    Res.ShowTime := CurrentTime;
    Res.ProjectionType := CurrentType;
    Res.SeatRow := CurrentSeatRow;
    Res.SeatCol := CurrentSeatCol;
    Res.PriceRSD := CalcPrice(CurrentType, CurrentTime);
    if cbGPay.Checked then Res.PaymentMethod := 'Google Pay'
    else if cbAPay.Checked then Res.PaymentMethod := 'Apple Pay'
    else Res.PaymentMethod := 'Kartica';
    Reservations.Add(Res);
  end;
end;

procedure TFormMain.RebuildReservationsList;
var
  I: Integer;
  Res: TReservation;
begin
  lstReservations.Clear;
  for I := 0 to Reservations.Count-1 do
  begin
    Res := Reservations[I];
    lstReservations.Items.AddObject(
      Format('%s | %s %s %s | %s | Sedиште %d-%d | %d RSD',
        [Res.MovieTitle, Res.City, Res.ShowDate, Res.ShowTime, Res.ProjectionType,
         Res.SeatRow+1, Res.SeatCol+1, Res.PriceRSD]), Res);
  end;
end;

procedure TFormMain.OnLogin(Sender: TObject);
begin
  if (edtUser.Text = 'admin') and (edtPass.Text = '1234') then
  begin
    lblLoginMsg.Caption := '';
    GoToTab(TabHome);
  end
  else
  begin
    lblLoginMsg.Caption := 'Нетачно корисничко име или лозинка.';
  end;
end;

procedure TFormMain.OnWatchMovie(Sender: TObject);
begin
  CurrentMovie := '';
  btnToOptionsFromMovie.Enabled := False;
  GoToTab(TabMovie);
end;

procedure TFormMain.OnGoMyReservations(Sender: TObject);
begin
  if Page.ActivePage = TabHome then
  begin
    RebuildReservationsList;
    GoToTab(TabMyRes);
  end
  else
  begin
    GoToTab(TabHome);
  end;
end;

procedure TFormMain.OnMoviePick(Sender: TObject);
var
  B: TButton;
begin
  B := Sender as TButton;
  CurrentMovie := B.Caption;
  btnToOptionsFromMovie.Enabled := True;
  GoToTab(TabOptions);
end;

procedure TFormMain.OnToSeats(Sender: TObject);
begin
  if cbKrag.Checked then CurrentCity := 'Kragujevac'
  else if cbBgd.Checked then CurrentCity := 'Beograd' else CurrentCity := '';

  if cbD1.Checked then CurrentDate := '10.12.2025'
  else if cbD2.Checked then CurrentDate := '20.12.2025' else CurrentDate := '';

  if cbT1.Checked then CurrentTime := '14:00'
  else if cbT2.Checked then CurrentTime := '20:00' else CurrentTime := '';

  if cbP1.Checked then CurrentType := '2D'
  else if cbP2.Checked then CurrentType := '3D' else CurrentType := '';

  if (CurrentMovie = '') or (CurrentCity = '') or (CurrentDate = '') or (CurrentTime = '') or (CurrentType = '') then
  begin
    ShowMessage('Молимо изаберите филм и све опције (град, датум, време, тип).');
    GoToTab(TabOptions);
    Exit;
  end;

  LoadTakenSeatsFromReservations;
  GoToTab(TabSeats);
end;

procedure TFormMain.OnReserveSeat(Sender: TObject);
var
  Price: Integer;
  Summary: string;
begin
  if (CurrentSeatRow < 0) or (CurrentSeatCol < 0) then
  begin
    ShowMessage('Изаберите седиште.');
    Exit;
  end;

  // prevent double booking
  if IsSeatAlreadyReserved(CurrentSeatRow, CurrentSeatCol, EditingReservation) then
  begin
    ShowMessage('Ово седиште је већ резервисано за исти филм/биоскоп/термин.');
    Exit;
  end;

  Price := CalcPrice(CurrentType, CurrentTime);

  Summary := Format('Филм: %s'#13#10'Град/датум/време: %s, %s, %s'#13#10'Тип: %s'#13#10'Седиште: %d-%d'#13#10'Цена: %d RSD',
    [CurrentMovie, CurrentCity, CurrentDate, CurrentTime, CurrentType, CurrentSeatRow+1, CurrentSeatCol+1, Price]);
  lblSummary.Caption := Summary;

  // anti-overlap – pomeri kontrole ispod teksta
  grpPayment.Top := lblSummary.Top + lblSummary.Height + 12;
  btnContinue.Top := grpPayment.Top + grpPayment.Height + 12;

  // reset payment bez rekurzije
  UpdatingPayment := True;
  try
    cbGPay.OnClick := nil; cbAPay.OnClick := nil; cbCard.OnClick := nil;
    cbGPay.Checked := False; cbAPay.Checked := False; cbCard.Checked := False;
  finally
    cbGPay.OnClick := OnlyOnePaymentChecked;
    cbAPay.OnClick := OnlyOnePaymentChecked;
    cbCard.OnClick := OnlyOnePaymentChecked;
    UpdatingPayment := False;
  end;

  GoToTab(TabCheckout);
end;

procedure TFormMain.OnContinue(Sender: TObject);
var
  PayChosen: Boolean;
begin
  PayChosen := cbGPay.Checked or cbAPay.Checked or cbCard.Checked;
  if not PayChosen then
  begin
    ShowMessage('Одаберите начин плаћања.');
    Exit;
  end;

  SaveCurrentAsReservation;
  EditingReservation := nil;
  GoToTab(TabHome);
end;

procedure TFormMain.OnOpenReservation(Sender: TObject);
var
  Idx: Integer;
  Res: TReservation;
begin
  Idx := lstReservations.ItemIndex;
  if Idx < 0 then
  begin
    ShowMessage('Одаберите резервацију.');
    Exit;
  end;

  Res := TReservation(lstReservations.Items.Objects[Idx]);

  lblResDetail.Caption := Format(
    'Филм: %s'#13#10'Град/датум/време: %s, %s, %s'#13#10'Тип: %s'#13#10'Седиште: %d-%d'#13#10'Цена: %d RSD'#13#10'Плаћање: %s',
    [Res.MovieTitle, Res.City, Res.ShowDate, Res.ShowTime, Res.ProjectionType, Res.SeatRow+1, Res.SeatCol+1, Res.PriceRSD, Res.PaymentMethod]);

  // poravnanje dugmadi ispod teksta
  btnChangeSeat.Top := lblResDetail.Top + lblResDetail.Height + 10;
  btnCancelRes.Top  := btnChangeSeat.Top;
  btnBackToMyRes.Top := btnChangeSeat.Top;

  EditingReservation := Res;

  GoToTab(TabResDetail);
end;

procedure TFormMain.OnChangeSeat(Sender: TObject);
begin
  if not Assigned(EditingReservation) then Exit;

  CurrentMovie := EditingReservation.MovieTitle;
  CurrentCity := EditingReservation.City;
  CurrentDate := EditingReservation.ShowDate;
  CurrentTime := EditingReservation.ShowTime;
  CurrentType := EditingReservation.ProjectionType;

  LoadTakenSeatsFromReservations;
  GoToTab(TabSeats);
end;

procedure TFormMain.OnCancelRes(Sender: TObject);
var
  I: Integer;
  Res: TReservation;
begin
  if not Assigned(EditingReservation) then Exit;

  for I := Reservations.Count-1 downto 0 do
  begin
    Res := Reservations[I];
    if Res = EditingReservation then
    begin
      Reservations.Delete(I);
      Break;
    end;
  end;

  EditingReservation := nil;
  RebuildReservationsList;
  GoToTab(TabMyRes);
end;

end.
