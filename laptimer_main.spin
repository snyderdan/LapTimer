CON

  _CLKMODE = XTAL1 + PLL16X
  _CLKFREQ = 80_000_000
  D1_SENSOR = 4
  D2_SENSOR = 5
  OE_PIN = 1 << 6
  LATCH_PIN = 1 << 7
  R1_PIN = 1 << 8
  R2_PIN = 1 << 9
  G1_PIN = 1 << 10
  G2_PIN = 1 << 11
  B1_PIN = 1 << 12
  B2_PIN = 1 << 13
  CLK_PIN = 1 << 14
  ADA_PIN = 1 << 15
  ADB_PIN = 1 << 16
  ADC_PIN = 1 << 17
  ADD_PIN = 1 << 18
  
VAR

  long SystemTime
  long pixels[256]
  
PUB Start | n
  ' Things that need to happen:
  '  1) Start time keeper core
  '  2) Start display driver core
  '  3) Start UI manager core
  '  4) Start driver 1 monitor core
  '  5) Start driver 2 monitor core
  '  6) Start monitoring for user input

  cognew(@time_keeper, @SystemTime)
  cognew(@display_driver, @pixels)

DAT TimeKeeper

            org     0
time_keeper
            mov     timeMSHub, PAR
            mov     nextPhase, CNT
            add     nextPhase, oneMS
sync_here
            waitcnt nextPhase, oneMS
            add     timeMS, 1
            wrlong  timeMS, timeMSHub
            jmp     sync_here

oneMS       long    _CLKFREQ / 1000
timeMS      long    0
nextPhase   res     1
timeMSHub   res     1

DAT DisplayDriver

            org     0
display_driver
            mov     pixeladdr, PAR
            mov     DIRA, allout    ' set output mode on all pins

            xor     OUTA, outenable ' disable output during row switch
            or      OUTA, latch     ' latch data from previous output during row switch
            ' do row checks
            ' ...
            
            or      OUTA, outenable ' enable output
            xor     OUTA, latch     ' latch down
            mov     batchcnt, #16
next_batch
            ' output rows
            rdlong  toprow, toprowaddr   
            mov     pixcnt, #4
            add     toprowaddr, #4
            
            rdlong  botrow, botrowaddr
            add     botrowaddr, #4
            nop
next_pix            
            shl     toprow, #1 wc
     if_c   or      OUTA, red1
            shl     botrow, #1 wc
     if_c   or      OUTA, red2
            
            shl     toprow, #1 wc
     if_c   or      OUTA, green1
            shl     toprow, #1 wc
     if_c   or      OUTA, blue1
     
            shl     botrow, #1 wc
     if_c   or      OUTA, green2
            shl     botrow, #1 wc
     if_c   or      OUTA, blue2
     
            or      OUTA, clock
            xor     OUTA, clock
            sub     pixcnt, #1 wz
     if_nz  jmp     next_pix
     
            sub     batchcnt, #1 wz
     if_nz  jmp     next_batch
             

allout      long    LATCH_PIN | OE_PIN | R1_PIN | R2_PIN | G1_PIN | G2_PIN | B1_PIN | B2_PIN | CLK_PIN | ADA_PIN | ADB_PIN | ADC_PIN | ADD_PIN
latch       long    LATCH_PIN
outenable   long    OE_PIN
red1        long    R1_PIN
red2        long    R2_PIN
green1      long    G1_PIN
green2      long    G2_PIN
blue1       long    B1_PIN
blue2       long    B2_PIN
clock       long    CLK_PIN
pixeladdr   res     1
batchcnt    res     1
pixcnt      res     1
toprow      res     1
botrow      res     1
toprowaddr  res     1
botrowaddr  res     1
            fit