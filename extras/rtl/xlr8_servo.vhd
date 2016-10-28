------------------------------------------------------------------------------- 
-- Filename    : xlr8_servo.vhd
-- Author      : Josh Rensch
--
-- Description :
--
--
--
-- © Copyright 2016, Superion Technology Group. All Rights Reserved
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_MISC.ALL;


entity xlr8_servo is
   generic (NUM_SERVOS : positive := 12;
            SVCR_ADDR  : std_logic_vector(7 downto 0) := (others => '0'); -- servo control register
            SVPWH_ADDR : std_logic_vector(7 downto 0) := (others => '0'); -- servo pulse width high
            SVPWL_ADDR : std_logic_vector(7 downto 0) := (others => '0')  -- servo pulse width low
      );
   
   port (
      clk  : in std_logic;
      en1mhz : in std_logic; -- clk enable at 1MHz rate
      rstn   : in std_logic;

      -- Register access for registers in first 64
      adr       : in std_logic_vector(5 downto 0);
      dbus_in   : in std_logic_vector(7 downto 0);
      dbus_out  : out std_logic_vector(7 downto 0);
      iore      : in std_logic;
      iowe      : in std_logic;
      io_out_en : out std_logic; -- was verilog wire.
      
      
      -- Register access for registers not in first 64 (all wires in verilog)
      ramadr    : in std_logic_vector(7 downto 0);
      ramre     : in std_logic;
      ramwe     : in std_logic;
      dm_sel    : in std_logic;
      
      -- External inputs/outputs
      servos_en  : out std_logic_vector(NUM_SERVOS-1 downto 0);
      servos_out : out std_logic_vector(NUM_SERVOS-1 downto 0)
   ) ;
end entity ; -- xlr8_servo

architecture rtl of xlr8_servo is


   type ChanPlsWidth is array (0 to NUM_SERVOS-1) of std_logic_vector(11 downto 0);
   constant ENABLE_ZERO : std_logic_vector(NUM_SERVOS-1 downto 0) := (others => '0');

   -- Registers in I/O address range x0-x3F (memory addresses -x20-0x5F)
   -- use the adr/iore/iowe inputs. Registers in the extended address
   -- range (memory address 0x60 and above) use ramadr/ramre/ramwe
   signal  CtlRegMemLoc       : std_logic := '0';
   signal  PlsWidthHighRegLoc : std_logic := '0';
   signal  PlsWidthLowRegLoc  : std_logic := '0';

   signal CtlRegSel          : std_logic;
   signal PlsWidthHighRegSel : std_logic;
   signal PlsWidthLowRegSel  : std_logic;

   signal CtlRegWrEn          : std_logic;
   signal PlsWidthHighRegWrEn : std_logic;
   signal PlsWidthLowRegWrEn  : std_logic;

   signal CtlRegRdEn          : std_logic;
   signal PlsWidthHighRegRdEn : std_logic;
   signal PlsWidthLowRegRdEn  : std_logic;

   signal TimerCnt     : std_logic_vector(14 downto 0);
   signal CtlReg       : std_logic_vector( 7 downto 0);
   signal ServosEnable : std_logic_vector(NUM_SERVOS-1 downto 0);
   signal ServosOut    : std_logic_vector(NUM_SERVOS-1 downto 0);

   signal PlsWidthHighReg : std_logic_vector(3 downto 0);
   signal PlsWidthLowReg  : std_logic_vector(7 downto 0);

   signal ChanPlsWidthArray : ChanPlsWidth;

   shared variable ChanIn : integer;
 
 

begin


   --------------------
   -- Control Register 
   --------------------  
   -- iF the address is higher than 0x60 then the information resides in memory, otherwise it resides
   -- in registers. This is determining where the address is. 
   CtlRegMemLoc <= '1' when (SVCR_ADDR(6 downto 5) = b"11") else '0';
   
   -- This is doing the seleection that the command coming in is for the control register.
   CtlRegSel    <= '1' when ((CtlRegMemLoc = '1' and dm_sel = '1' and (ramadr = SVCR_ADDR)) or 
                             (CtlRegMemLoc = '0' and (adr = SVCR_ADDR(5 downto 0))))
                       else '0';
   
   -- This determines whether the command is write or read. It checks to make sure the control register
   -- is selected and then hits the enable for the read or the write.  
   CtlRegWrEn   <= '1' when (CtlRegSel = '1' and 
                            ((CtlRegMemLoc = '1' and ramwe = '1') or (CtlRegMemLoc = '0' and iowe = '1'))) 
                       else '0';
   CtlRegRdEn   <= '1' when (CtlRegSel = '1' and 
                            ((CtlRegMemLoc = '1' and ramre = '1') or (CtlRegMemLoc = '0' and iore = '1'))) 
                       else '0';             


   ACTIVE_CHAN_TO_WORK_ON : process( dbus_in(4 downto 0) )
   begin
      if(to_integer(unsigned(dbus_in (4 downto 0))) >= NUM_SERVOS) then
         ChanIn := NUM_SERVOS - 1;
      else
         ChanIn := to_integer(unsigned(dbus_in (4 downto 0)));
      end if;
   end process ; -- ACTIVE_CHAN_TO_WORK_ON
   

   CTL_REG_UPDATE : process( clk, rstn )
   begin
      
      if (rstn = '0') then
         CtlReg <= x"00";
         ServosEnable <= ENABLE_ZERO;
      elsif (clk = '1' and clk'event) then
         if(CtlRegWrEn = '1') then
            CtlReg(4 downto 0) <= dbus_in (4 downto 0);
            CtlReg(7)            <= dbus_in(7) or (ServosEnable(ChanIn) and not dbus_in(6));
            ServosEnable(ChanIn) <= dbus_in(7) or (ServosEnable(ChanIn) and not dbus_in(6));         
         else
            CtlReg(7) <= ServosEnable(ChanIn);            
         end if;
      end if ;
   
   end process ; -- CTL_REG_UPDATE


   UPDATE_PULSE_WIDTH_ARRAY : process(clk, CtlRegWrEn)
   begin
      if (clk = '1' and clk'event and CtlRegWrEn = '1') then
         ChanPlsWidthArray(ChanIn) <= PlsWidthHighReg & PlsWidthLowReg;
      end if;

   end process ; -- UPDATE_PULSE_WIDTH_ARRAY





   --------------------
   -- Pulse High Register 
   --------------------  
   -- iF the address is higher than 0x60 then the information resides in memory, otherwise it resides
   -- in registers. This is determining where the address is. 
   PlsWidthHighRegLoc <= '1' when (SVPWH_ADDR(6 downto 5) = b"11") else '0';
   
   -- This is doing the seleection that the command coming in is for the control register.
   PlsWidthHighRegSel    <= '1' when ((PlsWidthHighRegLoc = '1' and dm_sel = '1' and (ramadr = SVPWH_ADDR)) or 
                             (PlsWidthHighRegLoc = '0' and (adr = SVPWH_ADDR(5 downto 0))))
                          else '0';
   
   -- This determines whether the command is write or read. It checks to make sure the control register
   -- is selected and then hits the enable for the read or the write.  
   PlsWidthHighRegWrEn <= '1' when (PlsWidthHighRegSel = '1' and 
                              ((PlsWidthHighRegLoc = '1' and ramwe = '1') or (PlsWidthHighRegLoc = '0' and iowe = '1'))) 
                       else '0';
   PlsWidthHighRegRdEn <= '1' when (PlsWidthHighRegSel = '1' and 
                              ((PlsWidthHighRegLoc = '1' and ramre = '1') or (PlsWidthHighRegLoc = '0' and iore = '1'))) 
                       else '0';   



   PULSE_WIDTH_HIGH_PROC : process( clk, rstn )
   begin
      if(rstn = '0') then
         PlsWidthHighReg <= x"0";
      elsif  (clk ='1' and clk'event) then
         if(PlsWidthHighRegWrEn = '1') then
            PlsWidthHighReg <= dbus_in(3 downto 0);
         end if;
      end if;     
   end process ; -- PULSE_WIDTH_HIGH_PROC



   --------------------
   -- Pulse Low Register 
   --------------------  
   -- iF the address is higher than 0x60 then the information resides in memory, otherwise it resides
   -- in registers. This is determining where the address is. 
   PlsWidthLowRegLoc <= '1' when (SVPWL_ADDR(6 downto 5) = b"11") else '0';
   
   -- This is doing the seleection that the command coming in is for the control register.
   PlsWidthLowRegSel    <= '1' when ((PlsWidthLowRegLoc = '1' and dm_sel = '1' and (ramadr = SVPWL_ADDR)) or 
                             (PlsWidthLowRegLoc = '0' and (adr = SVPWL_ADDR(5 downto 0))))
                          else '0';
   
   -- This determines whether the command is write or read. It checks to make sure the control register
   -- is selected and then hits the enable for the read or the write.  
   PlsWidthLowRegWrEn <= '1' when (PlsWidthLowRegSel = '1' and 
                              ((PlsWidthLowRegLoc = '1' and ramwe = '1') or (PlsWidthLowRegLoc = '0' and iowe = '1'))) 
                       else '0';
   PlsWidthLowRegRdEn <= '1' when (PlsWidthLowRegSel = '1' and 
                              ((PlsWidthLowRegLoc = '1' and ramre = '1') or (PlsWidthLowRegLoc = '0' and iore = '1'))) 
                       else '0';   


   PULSE_WIDTH_LOW_PROC : process( clk, rstn )
   begin
      if(rstn = '0') then
         PlsWidthLowReg <= x"00";
      elsif  (clk ='1' and clk'event) then
         if(PlsWidthLowRegWrEn = '1') then
            PlsWidthLowReg <= dbus_in;
         end if;
      end if;     
   end process ; -- PULSE_WIDTH_LOW_PROC

   
   --------------------
   -- Timer  
   --------------------  
   -- Simple timer that incriments from 0 to 20ms. It only incriments if the enable for the 1Mhz
   -- clk is going and at least one enable is active on one of the ports. 
   TIMER_PROC : process (clk, rstn)
   begin
      if(rstn = '0') then
         TimerCnt <= b"000" & x"000";
      elsif  (clk ='1' and clk'event) then
         if(to_integer(unsigned(TimerCnt)) >= 19999 ) then -- 19999 count or 20ms
            TimerCnt <= b"000" & x"000";
         elsif (en1mhz = '1' and or_reduce(ServosEnable) = '1') then
            TimerCnt <= TimerCnt + 1;
         end if;
      end if;
   
   end process; -- TIMER_PROC


   --------------------
   -- Generates Pulses to Servo 
   --------------------  
   -- This is creating a registered output for each possible servo signal this block is to create.
   -- The use of the intermediatary PlsON is a limitation of the VHDL. 
   GEN_PULSES_PER_CHAN : for I in 0 to NUM_SERVOS - 1 generate
      signal PlsOn : std_logic;

   begin
  
      PlsOn <= '1' when (to_integer(unsigned(TimerCnt(11 downto 0))) <= to_integer(unsigned(ChanPlsWidthArray(I)))) else
               '0';

      PULSE_PROC : process (clk, rstn)
      begin
         if(rstn = '0') then
            ServosOut(I) <= '0';
         elsif  (clk ='1' and clk'event) then
            ServosOut(I) <=  ServosEnable(I) and  PlsOn;
         end if; 
      end process; -- PULSE_PROC

   end generate GEN_PULSES_PER_CHAN;


   -- OUTPUTS
   servos_en   <= ServosEnable;
   servos_out  <= ServosOut;


   --------------------
   -- Reads Out Process
   --------------------  
   READ_OUT_PROC : process (CtlRegRdEn, PlsWidthHighRegRdEn, PlsWidthLowRegRdEn)
   begin
      if(CtlRegRdEn = '1') then 
         dbus_out <=  CtlReg(7) & b"00" & CtlReg(4 downto 0);
      elsif(PlsWidthHighRegRdEn = '1') then 
         dbus_out <=  b"0000" & PlsWidthHighReg;
      elsif (PlsWidthLowRegRdEn = '1') then
         dbus_out <= PlsWidthLowReg;
      else
         dbus_out <= x"00";
      end if;
   end process;

   io_out_en <= CtlRegRdEn or PlsWidthHighRegRdEn or PlsWidthLowRegRdEn;



end architecture ; -- rtl

