'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  SD_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile
$projecttime = 12


'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Procser()
Declare Sub Getdatetimeds3231()
Declare Sub Error(byval Genre As Byte)
Declare Sub Setdateds3231()
Declare Sub Settimeds3231()
Declare Sub Showclock()



'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte
Dim Lsyssec As Long
Dim Tmpl2 As Long
Dim Tmpw As Word

Dim Cmdtmp As String * 6
Dim Atsnd As String * 120
Dim Cmderr As Byte
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 52


'Variables TIMER0
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estado As Long
Dim Estado_led As Byte
Dim Iluminar As Bit
Dim Newseg As Bit

'CLK
Dim Mymonth As Byte
Dim Stringdow As String * 9       ' dow= day of week
Dim Dow As Byte
Dim Stringmonth As String * 10
Dim Jbyte As Byte , Kbyte As Byte , Oldsec As Byte
Dim Jword As Word

'MOTOR
Dim Giro As Byte
Dim Inigiro As Bit
Dim Numpulsos As Word
Dim Orgmotant As Bit
Dim Cntrgiros As Byte
Dim Cntrgirosant As Byte
Dim Inivuelta As Bit

'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Cmdsplit(6) As String * 20
Dim Serdata As String * 120 , Serrx As Byte , Serproc As String * 120



'*******************************************************************************
'* END public part                                                             *
'*******************************************************************************


Goto Loaded_arch

'*******************************************************************************
' INTERRUPCIONES
'*******************************************************************************

'*******************************************************************************
' Subrutina interrupcion de puerto serial 1
'*******************************************************************************
At_ser1:
   Serrx = Udr

   Select Case Serrx
      Case "$":
         Ser_ini = 1
         Serdata = ""

      Case 13:
         If Ser_ini = 1 Then
            Ser_ini = 0
            Serdata = Serdata + Chr(0)
            Serproc = Serdata
            Sernew = 1
            Enable Timer0
         End If

      Case Is > 31
         If Ser_ini = 1 Then
            Serdata = Serdata + Chr(serrx)
         End If

   End Select

Return


Return


'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer0:
   Timer0 = 184
   Incr T0c
   T0c = T0c Mod 8
   If T0c = 0 Then
      Num_ventana = Num_ventana Mod 32
      Estado = Lookup(estado_led , Tabla_estado)
      Iluminar = Estado.num_ventana
      Toggle Iluminar
      Led1 = Iluminar
      Led2 = Iluminar
      Incr Num_ventana
   End If

Return



Int_timer1:
   Timer1 = &H85EE
   Set Newseg
   Lsyssec = Syssec(time$ , Date$)
   Incr Lsyssec
   Time$ = Time(Lsyssec)
   Date$ = Date(lsyssec)
Return


Getdatetime:
Return

Setdate:
Return

Settime:
Return


'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************

'*******************************************************************************
' Inicialización de variables
'*******************************************************************************
Sub Inivar()
Reset Led1
Print #1 , "************ DS3231 ************"
Print #1 , Version(1)
Print #1 , Version(2)
Print #1 , Version(3)
Estado_led = 1


End Sub


'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   Print #1 , "$" ; Serproc
   Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   If Numpar > 0 Then
      For Tmpb = 1 To Numpar
         Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
      Next
   End If

   If Len(cmdsplit(1)) = 6 Then
      Cmdtmp = Cmdsplit(1)
      Cmdtmp = Ucase(cmdtmp)
      Cmderr = 255
      Select Case Cmdtmp
         Case "LEEVFW"
            Cmderr = 0
            Atsnd = "Version FW: Fecha <"
            Tmpstr52 = Version(1)
            Atsnd = Atsnd + Tmpstr52 + ">, Archivo <"
            Tmpstr52 = Version(3)
            Atsnd = Atsnd + Tmpstr52 + ">"


         Case "SETLED"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 17 Then
                  Cmderr = 0
                  Atsnd = "Se configura setled a " + Str(tmpb)
                  Estado_led = Tmpb

               Else
                  Cmderr = 5
               End If

            Else
               Cmderr = 4

            End If


         Case "SISCLK"
            Cmderr = 0
            Tmpstr52 = Time$
            'Print #1 , Tmpstr52
            'Print #1 , Time$
            Atsnd = "Hora actual=" + Tmpstr52 + ", Fecha actual="
            Tmpstr52 = Date$
            'Print #1 , Tmpstr52
            'Print #1 , Date$
            Atsnd = Atsnd + Tmpstr52

        Case "SETCLK"
            If Numpar = 2 Then
               If Len(cmdsplit(2)) = 10 Then
                  Cmderr = 0
                  Tmpstr52 = Mid(cmdsplit(2) , 7 , 2) + ":" + Mid(cmdsplit(2) , 9 , 2) + ":" + Mid(cmdsplit(2) , 11 , 2)
                  Print #1 , Tmpstr52
                  Time$ = Tmpstr52
                  Print #1 , "T>" ; Time$
                  Tmpstr52 = Mid(cmdsplit(2) , 1 , 2) + "/" + Mid(cmdsplit(2) , 3 , 2) + "/" + Mid(cmdsplit(2) , 5 , 2)
                  Print #1 , Tmpstr52
                  Date$ = Tmpstr52
                  Print #1 , "D>" ; Date$
                  Atsnd = "WATCHING INFORMA. Se configuro reloj en " + Date$ + " a " + Time$
                  'Horamin = Syssec()
                  'Horamineep = Horamin
                  Dow = Dayofweek()
                  Call Setdateds3231()
                  Call Settimeds3231()
                  Call Getdatetimeds3231()
                  'Set Actclkok
               Else
                  Cmderr = 6
               End If
            Else
               Cmderr = 4
            End If

         Case "LEERTC"
            Cmderr = 0
            Atsnd = "Lee RTC"
            Call Getdatetimeds3231()

         Case "SETMOT"
            If Numpar = 3 Then
               Giro = Val(cmdsplit(2))
               If Giro < 2 Then
                  Numpulsos = Val(cmdsplit(3))
                  Set Inigiro
                  Cmderr = 0
                  Atsnd = "Se configura Giro "
                  If Giro = 0 Then
                     Atsnd = Atsnd + "derecha "
                     Set Direc
                  Else
                     Atsnd = Atsnd + "izquierda "
                     Reset Direc
                  End If
                  Atsnd = Atsnd + Str(numpulsos) + " pulsos"
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETPAS"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 8 Then
                  Cmderr = 0
                  Atsnd = "Se config micropaso a " + Str(tmpb)
                  Ms1 = Tmpb.0
                  Ms2 = Tmpb.1
                  Ms3 = Tmpb.2
               Else
                  Cmderr = 4
               End If
            End If

         Case "LEEPAS"
            Tmpb = 0
            CMDERR=0
            Tmpb.0 = Ms1
            Tmpb.1 = Ms2
            Tmpb.2 = Ms3
            Atsnd = "Micropaso=" + Str(tmpb)

         Case Else
            Cmderr = 1

      End Select

   Else
        Cmderr = 2
   End If

   If Cmderr > 0 Then
      Atsnd = Lookupstr(cmderr , Tbl_err)
   End If

   Print #1 , Atsnd

End Sub

'*****************************************************************************
'---------routines I2C for  RTC DS3231----------------------------------------

'*****************************************************************************
Sub Getdatetimeds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 0                                             ' start address in 1307
     I2cstart                                               ' Generate start code
     If Err = 0 Then
        I2cwbyte Ds3231r                                    ' send address
        I2crbyte _sec , Ack
        I2crbyte _min , Ack       ' MINUTES
        I2crbyte _hour , Ack       ' Hours
        I2crbyte Dow , Ack                                        ' Day of Week
        I2crbyte _day , Ack                                       ' Day of Month
        I2crbyte _month , Ack       ' Month of Year
        I2crbyte _year , Nack       ' Year
        I2cstop
        If Err <> 0 Then
         Call Error(15)
        Else
           _sec = Makedec(_sec) : _min = Makedec(_min) : _hour = Makedec(_hour)
           _day = Makedec(_day) : _month = Makedec(_month) : _year = Makedec(_year)
        End If
     Else
      Print #1 , "No se encontro DS3231 en Getdatetime 2"
     End If
  Else
   Print #1 , "No se encontro DS3231 en Getdatetime 1"
  End If
End Sub
'-----------------------
Sub Setdateds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     _day = Makebcd(_day) : _month = Makebcd(_month) : _year = Makebcd(_year)
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 3                                                ' starting address in 1307
     I2cwbyte Dow
     I2cwbyte _day                                             ' Send Data to day
     I2cwbyte _month       ' Month
     I2cwbyte _year       ' Year
     I2cstop
     If Err <> 0 Then call Error(15)
  Else
   Print #1 , "No se encontro DS3231 en Setdate"
  End If
end sub
'-----------------------
Sub Settimeds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     _sec = Makebcd(_sec) : _min = Makebcd(_min) : _hour = Makebcd(_hour)
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 0                                                ' starting address in 1307
     I2cwbyte _sec                                             ' Send Data to SECONDS
     I2cwbyte _min                                             ' MINUTES
     I2cwbyte _hour                                         ' Hours
     I2cstop
     If Err <> 0 Then call Error(15)
  Else
   Print #1 , "No se encontro DS3231 en Settime"
  End If
 end sub
 '----------------------

 '********définition des erreurs***********************************************
Sub Error(byval genre As Byte )
Local Mes_error As String * 20
Select Case Genre
   Case 1
   Mes_error = " Reset  "
   Case 2
   Mes_error = " DFH "
   Case 3
   Mes_error = "set params  "
   Case 4
   Mes_error = "start "
  Case 5
   Mes_error = "Hardstop "
   Case 6
   Mes_error = "Status "
   Case 7
   Mes_error = "Getposition "
   Case 8
   Mes_error = "pas de module"
   Case 9
   Mes_error = "9xx"
   Case 10
   Mes_error = "10xx"
   Case 11
   Mes_error = "11xx"
   Case 12
   Mes_error = "12xx"
   Case 13
   Mes_error = "13xx"
   Case 14
   Mes_error = "ecriture clock"
   Case 15
   Mes_error = "lecture clock"
   Case Else
    Mes_error = "Autre erreur"
End Select
'Cls
Print #1 , "error=" ; Mes_error                             '; Adr_ax
'If Strerr <> "" Then
'   Locate 2 , 1 : Lcd Strerr
'End If
'Stop

End Sub


sub Showclock()
'   Cls
   Stringdow = Lookupstr(dow , Datadays)
   Mymonth = _month
   Stringmonth = Lookupstr(mymonth , Datamonth)
'   Locate 1 , 1 : Lcd Stringdow ; " " ;Date$
'   Locate 2 , 4 : Lcd Time$
   Print #1 , Stringdow ; "," ; Date$ ; "," ; Time$
   Oldsec = _sec
End Sub


'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************

Tbl_err:
Data "OK"                                                   '0
Data "Comando no reconocido"                                '1
Data "Longitud comando no valida"                           '2
Data "Numero de usuario no valido"                          '3
Data "Numero de parametros invalido"                        '4
Data "Error longitud parametro 1"                           '5
Data "Error longitud parametro 2"                           '6
Data "Parametro no valido"                                  '7
Data "ERROR8"                                               '8
Data "ERROR SD. Intente de nuevo"                           '9

Tabla_estado:
Data &B00000000000000000000000000000000&                    'Estado 0
Data &B00000000000000000000000000000011&                    'Estado 1
Data &B00000000000000000000000000110011&                    'Estado 2
Data &B00000000000000000000001100110011&                    'Estado 3
Data &B00000000000000000011001100110011&                    'Estado 4
Data &B00000000000000110011001100110011&                    'Estado 5
Data &B00000000000011001100000000110011&                    'Estado 6
Data &B00001111111111110000111111111111&                    'Estado 7
Data &B01010101010101010101010101010101&                    'Estado 8
Data &B00110011001100110011001100110011&                    'Estado 9
Data &B01110111011101110111011101110111&                    'Estado 10
Data &B11111111111111000000000000001100&                    'Estado 11
Data &B11111111111111000000000011001100&                    'Estado 12
Data &B11111111111111000000110011001100&                    'Estado 13
Data &B11111111111111001100110011001100&                    'Estado 14
Data &B11111111111111000000000000001100&                    'Estado 15
Data &B11111111111111111111111111110000&                    'Estado 16


      Datadays:
'Data " " , "LUNDI" , "MARDI" , "MERCREDI" , "JEUDI" , "VENDREDI" , "SAMEDI" , "DIMANCHE"       'one adds " " to start with idx=1
         Data " " , "LUN" , "MAR" , "MER" , "JEU" , "VEN" , "SAM" , "DIM"       'one adds " " to start with idx=1
      Datamonth:
'Data " " , "JANVIER" , "FEVRIER" , "MARS" , "AVRIL" , "MAI" , "JUIN" , "JUILLET" , "AOUT" , "SEPTEMBRE" , "OCTOBRE" , "NOVEMBRE" , "DECEMBRE"
         Data " " , "JAN" , "FEV" , "MAR" , "AVR" , "MAI" , "JUIN" , "JUIL" , "AOUT" , "SEPT" , "OCT" , "NOVE" , "DECE"


Loaded_arch: