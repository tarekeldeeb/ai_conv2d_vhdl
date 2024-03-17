----------------------------------------------------------------------------------
-- Author: Tarek ELDEEB
-- 
-- Create Date:    15:25:05 03/07/2019 
-- Design Name: 
-- Module Name:    matmul - Behavioral 
-- Project Name: 
-- Target Devices: 
-- License:  Check "LICENSE" which comes with this distribution for more informationg. 
-- Description: Data Pipe Runtime behavior, should be matched by the memory conroller
--  
--  MATMUL clk    |-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
--  MATMUL Output |--{New Address}----------------
--  RAMCTL change |__--___________________________
--  RAMCTL countr |__0,1,_________________________ << Count from 0,n-1 --> ]0
--  RAMCTL RdRq   |____0,1,_______________________ << Request a Burst n
--  MATMUL Valid  |______----_____________________ << May be delayed.
--  MATMUL FifEmp |________------_________________
--  MATMUL FifRd  |__________----_________________
--  MATMUL AB_val |____________----_______________ << MM Starts operation
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
use IEEE.math_real.all;
use ieee.std_logic_misc.all;

library matrix_mul_v1_00_a;
use matrix_mul_v1_00_a.ai_util_pkg.all;
--use UNISIM.VComponents.all;

entity matmul is
	 Generic (
			  n			: integer := 4; 	-- Number of PEs, and implicitly the matrix size of A and B (n*n).
			  width		: integer := 16); -- Data width, assuming signed integers
    Port ( clk 		: in  STD_LOGIC;
           rst 		: in  STD_LOGIC;
			  
			  -- Configurations
			  A_Address : in  STD_LOGIC_VECTOR(awidth-1 downto 0);
			  B_Address : in  STD_LOGIC_VECTOR(awidth-1 downto 0);
			  C_Address : in  STD_LOGIC_VECTOR(awidth-1 downto 0);
			  A_outerSz : in  STD_LOGIC_VECTOR(15 downto 0);
			  A_innerSz : in  STD_LOGIC_VECTOR(15 downto 0);
			  B_outerSz : in  STD_LOGIC_VECTOR(15 downto 0);
			  
			  -- CSR
           Command 	: in  STD_LOGIC_VECTOR(15 downto 0);
			  Status    : out STD_LOGIC_VECTOR(15 downto 0);
			  interrupt : out STD_LOGIC;

			  -- Data Pipes
			  IA_Address: out STD_LOGIC_VECTOR(awidth-1 downto 0);
			  IA_Burst	: out STD_LOGIC_VECTOR(15 downto 0);
			  IA_Valid  : in  STD_LOGIC;
			  IA_Data   : in  STD_LOGIC_VECTOR(width-1 downto 0);
			  IB_Address: out STD_LOGIC_VECTOR(awidth-1 downto 0);
			  IB_Burst  : out STD_LOGIC_VECTOR(15 downto 0);
			  IB_Valid  : in  STD_LOGIC;
			  IB_Data   : in  STD_LOGIC_VECTOR(width-1 downto 0);
			  OC_Address: out STD_LOGIC_VECTOR(awidth-1 downto 0);
			  OC_Valid  : out STD_LOGIC;
			  OC_Data   : out STD_LOGIC_VECTOR(width-1 downto 0)
			  );
end matmul;

architecture Behavioral of matmul is

  constant max_input_size	: integer:=4096;
  constant counter_width	: integer:=16;--ilog2(max_input_size/n); --TODO: Handle!

  SIGNAL AB_val					: STD_LOGIC:='0';
  SIGNAL Cout_val					: STD_LOGIC;
  SIGNAL AB_count             : unsigned(ilog2(n+1)-1 downto 0):=to_unsigned(n,ilog2(n+1));
  SIGNAL OCC_count, OCR_count : unsigned(ilog2(n+1)-1 downto 0):=to_unsigned(n,ilog2(n+1));
  SIGNAL rst_mm, rst_C_mm     : STD_LOGIC;
  SIGNAL rst_pe,rst_pe_c      : STD_LOGIC;
  SIGNAL run_complete         : STD_LOGIC;

  SIGNAL Empty_A,Empty_B		: STD_LOGIC;
  SIGNAL Read_A, Read_B			: STD_LOGIC;
  SIGNAL A, B, C	   			: STD_LOGIC_VECTOR (width-1 downto 0);
  SIGNAL A_memData, B_memData : STD_LOGIC_VECTOR (width-1 downto 0);
  
  SIGNAL comm	: t_command;
  SIGNAL conf	: t_config;
  SIGNAL stat	: t_status:=(reserved=>(others=>'0'),error=>(others=>'0'), error_size=>(others=>'0'),busy=>"0");
  SIGNAL oper	: std_logic_vector(comm.operation'range);
  
  TYPE t_State IS 	(idle, latch, check, error, run);
  SIGNAL state 		: t_State;

  SIGNAL MM_burst_run           : STD_LOGIC;

  SIGNAL MM_patch_run           : STD_LOGIC;
  SIGNAL MM_patch_AB_val        : STD_LOGIC;
  SIGNAL MM_patch_Read_A_o      : STD_LOGIC;
  SIGNAL MM_patch_Read_B_o      : STD_LOGIC;
  SIGNAL MM_patch_rst_pe        : STD_LOGIC:='0';
  SIGNAL MM_patch_rst_pe_c      : STD_LOGIC:='0';
  SIGNAL MM_patch_IA_Address    : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL MM_patch_IB_Address    : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL MM_patch_OC_Address    : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL MM_patch_run_complete  : STD_LOGIC:='0';
  
  SIGNAL MM_transpose_run           : STD_LOGIC;
  SIGNAL MM_transpose_AB_val        : STD_LOGIC;
  SIGNAL MM_transpose_Read_A_o      : STD_LOGIC;
  SIGNAL MM_transpose_Read_B_o      : STD_LOGIC;
  SIGNAL MM_transpose_rst_pe        : STD_LOGIC:='0';
  SIGNAL MM_transpose_rst_pe_c      : STD_LOGIC:='0';
  SIGNAL MM_transpose_IA_Address    : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL MM_transpose_IB_Address    : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL MM_transpose_OC_Address    : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL MM_transpose_run_complete  : STD_LOGIC:='0';
  SIGNAL MM_transpose_B_Data			: STD_LOGIC_VECTOR (width-1 downto 0);
  
  SIGNAL MM_in      						: t_mem_control_in;
  SIGNAL MM_patch_out, MM_burst_out : t_mem_control_out;
  SIGNAL MM_transpose_out, MM_out   : t_mem_control_out;
  
begin

MUL: entity matrix_mul_v1_00_a.MM Generic Map (n=>n, width=>width)
        Port Map (
		     clk 		=> clk,
           rst 		=> rst_mm,
           rst_C 		=> rst_C_mm,
           A 			=> A,              -- Feed-in Column-based
           B 			=> B,              -- Feed-in Row-based
			  AB_val		=> AB_val,         -- Both A and B are valid inputs (TODO: Validate with real memory)
			  AB_count  => AB_count,       -- N in all cases, except remainders
			  Cout_Rows	=> oper(OPR_BIT_T),-- Select if the output is required in Row-based (transposed)
			  OCC_count => OCC_count,      -- N in all cases, except remainders
			  OCR_count => OCR_count,      -- N in all cases, except remainders
			  C_out 		=> C,              -- C stream GEMM result
			  Cout_val 	=> Cout_val        -- Result C is valid
		  );

A 	<= A_memData;
B	<= B_memData when oper(OPR_BIT_S downto OPR_BIT_T) /= "101" else
      MM_transpose_out.B_Data; -- Identity Matrix: AxI => A'
      
Fifo_A: entity matrix_mul_v1_00_a.fifo 
				 Generic Map ( DATA_WIDTH => width, FIFO_DEPTH => 2*n)
				 Port Map ( clk, rst, IA_Valid, IA_Data, Read_A, A_memData, Empty_A, open);
Fifo_B: entity matrix_mul_v1_00_a.fifo 
             Generic Map ( DATA_WIDTH => width, FIFO_DEPTH => 2*n)
				 Port Map ( clk, rst, IB_Valid, IB_Data, Read_B, B_memData, Empty_B, open);

mem_burst: entity matrix_mul_v1_00_a.matmul_burst_control
   Generic Map(
        n        => n,
        width    => width)
    Port Map( 
        clk            => clk,
        rst            => rst,
		  run 			  => MM_burst_run,
        cntIn			  => MM_in,
        cntOut			  => MM_burst_out
        );
		  
mem_patch: entity matrix_mul_v1_00_a.matmul_patchify_control
   Generic Map(
        n        => n,
        width    => width)
    Port Map( 
        clk            => clk,
        rst            => rst,
		  run 			  => MM_patch_run,
        cntIn			  => MM_in,
        cntOut			  => MM_patch_out
        );

mem_trans: entity matrix_mul_v1_00_a.matmul_transpose_control
   Generic Map(
        n        => n,
        width    => width)
    Port Map( 
        clk            => clk,
        rst            => rst,
		  run 			  => MM_transpose_run,
        cntIn			  => MM_in,
        cntOut			  => MM_transpose_out
        );
		  
rst_mm		  <= rst OR rst_pe;
rst_C_mm		  <= rst OR rst_pe_c;
			  
OC_Valid  	  <= Cout_val;
OC_Data   	  <= C;

comm	        <= slv_to_t_command(Command);
Status        <= t_status_to_slv(stat);

MM_in	   <= (comm=>comm,conf=>conf,Cout_val=>Cout_val,Empty_A=>Empty_A,Empty_B=>Empty_B); 
MM_out	<= MM_burst_out     when MM_burst_run = '1' else
				MM_transpose_out when MM_transpose_run = '1' else
				MM_patch_out     when MM_patch_run = '1' else
				(  (others=>'0'),(others=>'0'),
				   '0','0','0','0','0','0',
					(others=>'0'),(others=>'0'),(others=>'0'),(others=>'0'),
					(others=>'0'),(others=>'0'),(others=>'0'),(others=>'0')
				);

AB_val        <= MM_out.AB_val;
Read_A        <= MM_out.Read_A;
Read_B        <= MM_out.Read_B;
rst_pe        <= MM_out.rst_pe;
rst_pe_c      <= MM_out.rst_pe_c;
IA_Address    <= MM_out.IA_Address;
IA_Burst		  <= MM_out.IA_Burst;
IB_Address    <= MM_out.IB_Address;
IB_Burst		  <= MM_out.IB_Burst;
OC_Address    <= MM_out.OC_Address;
OCC_count    <= resize(unsigned(MM_out.OCC_count), ilog2(n+1));
OCR_count    <= resize(unsigned(MM_out.OCR_count), ilog2(n+1));
AB_count      <= resize(unsigned(MM_out.AB_count), ilog2(n+1));
run_complete  <= MM_out.run_complete;

state_mch : process (clk,rst)
begin
  if rst='1' then
    state		       <= idle;
	 conf			       <= ((others=>'0'),(others=>'0'),(others=>'0'),(others=>'0'),(others=>'0'),(others=>'0'));
	 interrupt	       <= '0';
	 oper			       <= (others=>'0');
	 MM_burst_run      <= '0';
	 MM_patch_run      <= '0';
	 MM_transpose_run  <= '0';
  elsif rising_edge(clk) then
    Case state is
		 when idle =>
		   MM_burst_run	    <= '0';
		   MM_patch_run       <= '0';
			MM_transpose_run   <= '0';
			if comm.run = "1" then
				state	<= check;
				conf	<= slv_to_t_config(A_Address & B_Address & C_Address &
												 A_outerSz & A_innerSz & B_outerSz);
				oper	<= comm.operation;				
			end if;
		 when check =>
			if Command(OPR_EXP'range) /= OPR_EXP
 			   --conf.A_outerSz mod n = 0 AND 
				--(conf.A_innerSz mod n = 0 OR Command(OPR_BIT_CV) = '1')
				-- AND (conf.B_outerSz mod n = 0 OR Command(OPR_BIT_CV) = '1') --TODO Remove !
				then
				state	      <= run;
				stat.busy	<= "1";				
			else
				state					<= error;
				--stat.error_size	<= "1";
				stat.error	      <= "1";
				interrupt			<= '1';				
			end if;
		 when error =>
			if comm.clear_err = "1" then
			  state 	<= idle;
			  stat.error_size		<= "0";
			  stat.error		   <= "0";
			  interrupt				<= '0';				
			end if;
		 when run =>
		   if oper(OPR_BIT_S downto OPR_BIT_CV) = "00" then
				MM_burst_run	<= '1';
         elsif oper(OPR_BIT_CV) = '1' then 
				MM_patch_run <= '1';
			elsif oper(OPR_BIT_S downto OPR_BIT_T) = "101" then 
			   MM_transpose_run <= '1';
			else
			   report "Unknown Instruction!" severity error;
			   --TODO: Handle Unknown instruction!
			end if;
		   if run_complete = '1' then
			  state	<= idle;
			  stat.busy	<= "0";				
			end if;
		 when others =>
			state	<= idle;
	 End Case;
  end if;
end process;

end Behavioral;
