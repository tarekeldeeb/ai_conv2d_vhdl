----------------------------------------------------------------------------------
-- Author: Tarek ELDEEB
-- 
-- Create Date:    14:09:17 09/29/2019 
-- Design Name: 
-- Module Name:    matmul_memory - Behavioral 
-- License:  Check "LICENSE" which comes with this distribution for more informationg.
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
----------------------------------------------------------------------------------
  LIBRARY ieee;
  use ieee.math_real.all;
  USE ieee.std_logic_1164.ALL;
  USE ieee.numeric_std.ALL;

  library matrix_mul_v1_00_a;
  use matrix_mul_v1_00_a.ai_util_pkg.all;

entity matmul_memory is
  	 Generic (
  	   n        : integer:=4;
	   k        : integer:=3;    -- Kernel size KxK
	   size		: integer:=mem_size_max; -- Max 4096
		width		: integer := 16; -- Data width, assuming signed integers, Max 32
		init		:  mem_init_t;
		rnd_delay: boolean:=false);  
    Port ( 
	   clk		: in  STD_LOGIC;
		-- synthesis translate_off
		reinit	: mem_init_t;
      -- synthesis translate_on 
		Patchify : in  STD_LOGIC; -- Load A as kernel
		PadSame  : in  STD_LOGIC; -- Padding = 'Same'? Otherwise 'No padding'.
		A_Address: in  STD_LOGIC_VECTOR(awidth-1 downto 0);
		A_Burst  : in  STD_LOGIC_VECTOR(15 downto 0);
		A_Valid  : out STD_LOGIC:='0';
		A_Data   : out STD_LOGIC_VECTOR(width-1 downto 0);
		B_Address: in  STD_LOGIC_VECTOR(awidth-1 downto 0);
	   B_Burst  : STD_LOGIC_VECTOR(15 downto 0);
		B_Valid  : out STD_LOGIC;
		B_Data   : out STD_LOGIC_VECTOR(width-1 downto 0);
		C_Address: in  STD_LOGIC_VECTOR(awidth-1 downto 0);
		C_Valid  : in  STD_LOGIC;
		C_Data   : in  STD_LOGIC_VECTOR(width-1 downto 0)
	);
end matmul_memory;

architecture Behavioral of matmul_memory is
  Constant Ksq										: integer:=(k*k);

  TYPE t_State IS 	(idle, loadingKernel, run);
  Signal state 		: t_State:=idle;
  
  -- TODO: Implement a read memory Cache
  -- TODO: Support Multiple kernels in parallel
  type t_kernel is array (0 to Ksq-1) of std_logic_vector(width-1 downto 0);
  Signal kernel		: t_kernel:=(others=>(others=>'0'));

  Signal addr_a, addr_b, addr_c				: unsigned(ilog2(size) -1 downto 0):=(others=>'0');
  Signal cnt_a, cnt_c					      : unsigned(ilog2( max(n,max_burst) ) -1 downto 0):=(others=>'0');
  Signal cnt_b					               : unsigned(ilog2( max(n,k) ) -1 downto 0):=(others=>'0');
  Signal cnt_ksq									: unsigned(ilog2( Ksq ) -1 downto 0):=(others=>'0');
  Signal cnt_k									   : unsigned(ilog2( K ) -1 downto 0):=(others=>'0');
  Signal A_Address_reg, B_Address_reg		: STD_LOGIC_VECTOR(awidth-1 downto 0):=(others=>'0');
  Signal C_Address_reg							: STD_LOGIC_VECTOR(awidth-1 downto 0):=(others=>'0');
  Signal addr_a_ch, addr_b_ch, addr_c_ch	: std_logic:='0';
  Signal rAValid, rBValid, rCValid			: std_logic:='0';
  Signal rCValid2			                  : std_logic:='0';
  Signal rCData, rCData2						: std_logic_vector(width-1 downto 0);
  Signal rand_num,rand_num_reg			   : integer := 0; --Random 0:9
  Signal rand_num_a,rand_num_b			   : integer := 0; 
  Signal rAddr_a_ch, rAddr_b_ch				: std_logic_vector(9 downto 0);
  Signal rA_Data            					: STD_LOGIC_VECTOR(width-1 downto 0);
  Signal K_Valid   								: STD_LOGIC:='0';

  begin
	
  state_proc: process(clk)
begin
  if rising_edge(clk) then
	 if Patchify = '1' then
		  Case state is
			 when idle =>
				if rAddr_a_ch(rand_num_a) = '1' then
				  state	<= loadingKernel;
				end if;
			 when loadingKernel =>
			   K_Valid   <= rAValid;
				if K_Valid = '1' then
				  kernel(Ksq-1)	<= rA_Data;
				  for i in 0 to Ksq-2 loop
					 kernel(i)	<= kernel(i+1);
				  end loop;
				end if;  
				if cnt_a = 0 and K_Valid = '0' then 
				  state	<= run;
				end if;
			 when run =>
				A_Valid   	<= '0';
				if K_Valid = '1' then --TODO: FIXME
				  A_Valid   	<= '1';
				  kernel(Ksq-1)<= kernel(0);
				  for i in 0 to Ksq-2 loop
					 kernel(i)	<= kernel(i+1);
				  end loop;
				end if;  
				if rAddr_a_ch(rand_num_a) = '1' then --TODO
				  state	<= idle;
				end if;
			 when others =>
					state 	<= idle;
			end Case;
		else --Normal Matmul
			state		<= idle;
			A_Valid	<= rAValid;
		end if;
  end if;
end process;

	ram: entity matrix_mul_v1_00_a.true_dpram_sclk 
	generic map(
      width		=> width,
		size		=> size,
		init 		=> init)
	port map
	(	
		clk		=> clk,
		-- synthesis translate_off
		reinit	=> reinit,
      -- synthesis translate_on 
		data_c	=> rCData2,
		addr_c	=> to_integer(unsigned(addr_c)),
		we_c		=> rCValid2,
		addr_a	=> to_integer(unsigned(addr_a)),
		addr_b	=> to_integer(unsigned(addr_b)),
		q_a		=> rA_Data,
		q_b		=> B_Data
	);

  A_Data 	<= rA_Data when Patchify = '0' else
					kernel(0);

  addressChanged: process(clk)
  begin
	  if rising_edge(clk) then
			A_Address_reg		<= A_Address;
			B_Address_reg		<= B_Address;
			C_Address_reg		<= C_Address;
			rand_num_reg		<= rand_num;
			if A_Address /= (A_Address'range=>'0') and A_Address /= A_Address_reg then
				addr_a_ch 		<= '1';
				rand_num_a	   <= rand_num;
			else
				addr_a_ch <= '0';
			end if;
			if B_Address /= (B_Address'range=>'0') and B_Address /= B_Address_reg then
				addr_b_ch 		<= '1';
				rand_num_b		<= rand_num_reg;
			else
				addr_b_ch <= '0';
			end if;
			if C_Address /= (C_Address'range=>'0') and C_Address /= C_Address_reg then
				addr_c_ch <= '1';
			else
				addr_c_ch <= '0';
			end if;
		end if;
  End process;
  
  feed_n: process(clk)
  begin
	 if rising_edge(clk) then
		-- A port
		 if rAddr_a_ch(rand_num_a) = '1' then
				cnt_a		<= unsigned(A_Burst(cnt_a'range))-1;
				addr_a	<= resize(unsigned(A_Address),addr_a'length);
				rAValid	<= '1'; --Check!?
		 elsif cnt_a /= (cnt_a'range=>'0') then
				cnt_a		<= (cnt_a - 1);				
				addr_a	<= (addr_a + 1);
				rAValid	<= '1';
		 else
				cnt_a		<= (others=>'0');
				cnt_ksq	<= (others=>'0');
				addr_a	<= (others=>'0');
				rAValid	<= '0';	 
		 end if;

	   -- B port
		 if rAddr_b_ch(rand_num_b) = '1' then
				cnt_b		<= unsigned(B_Burst(cnt_b'range))-1;
				addr_b	<= resize(unsigned(B_Address),addr_b'length);
				rBValid	<= '1'; --Check!?
		 elsif cnt_b /= (cnt_b'range=>'0') then
				cnt_b		<= (cnt_b - 1);
				addr_b	<= (addr_b + 1);
				rBValid	<= '1';
		 else
				cnt_b		<= (others=>'0');
				addr_b	<= (others=>'0');
				rBValid	<= '0';	 
		 end if;
		 B_Valid	<= rBValid;

	   -- C port
		 if addr_c_ch = '1' then
				cnt_c		<= to_unsigned(terni( Patchify='1' ,Ksq-1,n-1),cnt_c'length);
				addr_c	<= resize(unsigned(C_Address),addr_c'length);
		 elsif cnt_c /= (cnt_c'range=>'0') AND rCValid2='1' then
				cnt_c		<= (cnt_c - 1);
				addr_c	<= (addr_c + 1);
		 end if;
		 rCData	<= C_Data;
		 rCData2	<= rCData;
		 rCValid	<= C_Valid;
		 rCValid2<= rCValid;
	 End if;
  End process;
  
   -- synthesis translate_off
	rnd: if rnd_delay generate
		process(clk)
			variable seed1, seed2: positive;       -- seed values for random generator
			variable rand: real;  						 -- random real-number value in range 0 to 1.0  
			variable range_of_rand : real := 9.0;  -- the range of random values created will be 0 to +1000.
		begin
			if rising_edge(clk) then
			 uniform(seed1, seed2, rand);                -- generate random number
			 rand_num <= integer(rand*range_of_rand);  -- rescale to 0..9, convert integer part
			end if;
		end process;
		
		shift_mema: entity matrix_mul_v1_00_a.shiftreg 
								generic map(10) port map(clk,addr_a_ch,rAddr_a_ch);
		shift_memb: entity matrix_mul_v1_00_a.shiftreg 
								generic map(10) port map(clk,addr_b_ch,rAddr_b_ch);
   end generate;								
	-- synthesis translate_on
	
	no_rnd: if not rnd_delay generate
		rAddr_a_ch(0)	<= addr_a_ch;
		rAddr_b_ch(0)	<= addr_b_ch;
	end generate;

end Behavioral;

