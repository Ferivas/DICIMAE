'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para verificar subrutinas de reloj DS3231 en ATMEGA328PB
'
' Utiliza Timer1 para generar interrupcionesde 1 HZ


$version 0 , 1 , 9
$regfile = "m1284Pdef.dat"
$crystal = 7372800
$baud = 9600


$hwstack = 128
$swstack = 128
$framesize = 128
$projecttime = 40


'Declaracion de constantes
Const DS3231r = &B11010001  'DS3231 is very similar to DS1307 but it include a precise crystal
Const Ds3231w = &B11010000
Const Delaymotor = 5


'Configuracion de entradas/salidas
'Configuracion de entradas/salidas
Led1 Alias Portc.2                                          'LED VERDE
Config Led1 = Output

Led2 Alias Portc.3                                          'LED AZUL
Config Led2 = Output

'Buzzer
Buzzer Alias Porta.2
Config Buzzer = Output

Set Buzzer

'DRV MOTOR

Enadrv Alias Portb.0
Config Enadrv = Output

Ms1 Alias Portb.1
Config Ms1 = Output

Ms2 Alias Portb.2
Config Ms2 = Output

Ms3 Alias Portb.3
Config Ms3 = Output

Pulso Alias Portb.4
Config Pulso = Output

Direc Alias Portd.4
Config Direc = Output

Enaorg Alias Portd.5
Config Enaorg = Output


Orgmot Alias Pinc.5
Config Orgmot = Input
Set Portc.5

Snrfood Alias Pina.7
Config Snrfood = Input
Set Porta.7

Enasnrfood Alias Porta.6
Config Enasnrfood = Output

'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer0 Int_timer0
Enable Timer0
Start Timer0

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

Config Clock = User
Config Date = Dmy , Separator = /

Config Sda = Portc.1
Config Scl = Portc.0
Config I2cdelay = 10
I2cinit
'Set Portc.4
'Set Portc.5                                                 ' the DS3231 is working only to 100kHz but it's work to 200kHz

'TIMER1
Config Timer1 = Timer , Prescale = 256                      'Ints a 100Hz si Timer0=184
On Timer1 Int_timer1
Enable Timer1
Start Timer1


Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "DICIMAE_archivos.bas"



'Programa principal

Call Inivar()

   Print #1 , "Leo DS3231"
   Print #1 , "DDRC=" ; Bin(ddrc)
   Print #1 , "pinC=" ; Bin(pinc)
   Call Getdatetimeds3231()

   If Err = 0 Then
      Print #1 , "RTC Hora=" ; Time$ ; ",Fecha=" ; Date$
      Tmpl2 = Syssec()
      If Tmpl2 > 583584521 Then
         Print #1 , "Hora valida, no es necesario ACTCLK"
         Estado_led = 1
      End If
   Else
      Print # 1 , "ERROR CLK"
      Estado_led = 2

   End If


Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Inigiro = 1 Then
      Reset Inigiro
      Print #1 , "GIRO mot"
      Reset Enadrv
      Reset Pulso
      Set Enaorg
      Cntrgiros = 0
      For Tmpw = 1 To Numpulsos
         Set Pulso
         Waitus 10
         Reset Pulso
         Waitms Delaymotor
      Next
      Set Enadrv
      Reset Enaorg
      Print #1 , "Fin Giro"
   End If

Loop