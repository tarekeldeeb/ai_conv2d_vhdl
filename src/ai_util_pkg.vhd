	-------------------------------------------------------------------------------
-- Author: Ahmed Zaazaa
--
-- Create Date:    18.11.2018
-- Design Name:
-- Module Name:    ai_util_pkg - Package
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library matrix_mul_v1_00_a;

package ai_util_pkg is
  -- Constants
  constant width		    : integer := 16;  -- Memory/Data width
  constant awidth		    : integer := 32;  -- Address width
  constant max_burst     : integer := 256;
  constant counter_width : integer := 16;  -- Inner counters and Dimensions
  constant mem_size_max  : integer := 1024; -- May be set to 4096 MAX

  constant OPR_MM		: std_logic_vector(13 downto 2):=X"000"; -- Matrix-Multiplication
  constant OPR_MMT	: std_logic_vector(13 downto 2):=X"001"; -- Matrix-Multiplication, C'
  constant OPR_CV		: std_logic_vector(13 downto 2):=X"002"; -- Conv2D (Valid)
  constant OPR_CVT	: std_logic_vector(13 downto 2):=X"003"; -- Conv2D (Valid), C'
  constant OPR_TRP	: std_logic_vector(13 downto 2):=X"005"; -- Transpose: 4x6 => 6x4
  constant OPR_CVS	: std_logic_vector(13 downto 2):=X"006"; -- Conv2D (Same)
  constant OPR_CVST	: std_logic_vector(13 downto 2):=X"007"; -- Conv2D (Same), C'
  constant OPR_RTRP	: std_logic_vector(13 downto 2):=X"00D"; -- Reshape+Transpose: 4x6 => 4x6 
  constant OPR_EXP	: std_logic_vector(13 downto 2):=X"010"; -- Exception/Trap 
  
  constant OPR_BIT_T : integer:=2; --Transpose Output bit
  constant OPR_BIT_CV: integer:=3; --ConVolution bit 
  constant OPR_BIT_S : integer:=4; --Same bit
  constant OPR_BIT_R : integer:=5; --Reshape bit
  
  -- Types and Records ---------
  type t_command is record
    reserved  	: std_logic_vector(15 downto 14);
	 operation	: std_logic_vector(13 downto 2);
	 clear_err	: std_logic_vector(1 downto 1);
    run    		: std_logic_vector(0 downto 0);
  end record t_command;
  type t_status is record
    reserved  	: std_logic_vector(15 downto 3);
    error	   : std_logic_vector(2 downto 2);
    error_size	: std_logic_vector(1 downto 1);
    busy   		: std_logic_vector(0 downto 0);
  end record t_status;
  type t_config is record
	 A_Address : STD_LOGIC_VECTOR(3*awidth+47 downto 2*awidth+48);
	 B_Address : STD_LOGIC_VECTOR(2*awidth+47 downto awidth+48);
	 C_Address : STD_LOGIC_VECTOR(awidth+47   downto 48);
	 A_outerSz : unsigned(47 downto 32);
	 A_innerSz : unsigned(31 downto 16);
	 B_outerSz : unsigned(15 downto 0);
  end record t_config;
  type t_mem_control_in is record
    comm				  : t_command;
    conf            : t_config;
    Cout_val        : STD_LOGIC;
    Empty_A         : STD_LOGIC;
    Empty_B         : STD_LOGIC;
  end record t_mem_control_in;
  type t_mem_control_out is record
    A_Data  	     : std_logic_vector(width-1 downto 0);
    B_Data  	     : std_logic_vector(width-1 downto 0);
    AB_val          : STD_LOGIC;
    Read_A          : STD_LOGIC;
    Read_B          : STD_LOGIC;
    rst_pe          : STD_LOGIC;
    rst_pe_c        : STD_LOGIC;
    run_complete    : STD_LOGIC;
    IA_Address      : STD_LOGIC_VECTOR(awidth-1 downto 0);
	 IA_Burst		  : STD_LOGIC_VECTOR(15 downto 0);
    IB_Address      : STD_LOGIC_VECTOR(awidth-1 downto 0);
	 IB_Burst		  : STD_LOGIC_VECTOR(15 downto 0);
    OC_Address      : STD_LOGIC_VECTOR(awidth-1 downto 0);
	 OCC_count       : STD_LOGIC_VECTOR(counter_width-1 downto 0);
	 OCR_count       : STD_LOGIC_VECTOR(counter_width-1 downto 0);
	 AB_count        : STD_LOGIC_VECTOR(counter_width-1 downto 0);
  end record t_mem_control_out;
  TYPE mem_init_t IS ARRAY(0 TO 4095) OF std_logic_vector(31 DOWNTO 0);
  type t_testcase is record
	 mem_init  : mem_init_t;
	 mem_size  : integer;
	 configur  : t_config;
  end record t_testcase;

  -- Functions and Procedures --
  function ilog2(val : integer) return integer ;
  function max(val1 : integer; val2 : integer) return integer ;
  function min(val1 : integer; val2 : integer) return integer ;
  function umax(val1 : unsigned; val2 : unsigned) return unsigned ;
  function umin(val1 : unsigned; val2 : unsigned) return unsigned ;
  function t_status_to_slv(L: t_status) return std_logic_vector;
  function slv_to_t_status(L: std_logic_vector(15 downto 0)) return t_status;
  function slv_to_t_command(L: std_logic_vector(15 downto 0)) return t_command;
  function slv_to_t_config(L: std_logic_vector(3*awidth+47 downto 0)) return t_config; 
  function ternu(cond : boolean; res_true, res_false : unsigned) return unsigned;
  function terni(cond : boolean; res_true, res_false : integer) return integer;
  function rr(L: std_logic_vector) return std_logic_vector;
  function umult_same( a, b : unsigned) return unsigned;
  function uceildiv( a, b : unsigned) return unsigned;
  
  procedure config_to_vecs(signal conf: in t_config; signal aadr, badr, cadr : out std_logic_vector(awidth-1 downto 0); signal aoSz,aiSz,boSz : out unsigned(15 downto 0) );
  procedure pauseSignal( signal sig : inout std_logic; period : in time; cond : in boolean);
end package;

package body ai_util_pkg is

  function ilog2(val : integer) return integer is
    variable result : integer;
  begin
    if val = 0 then
      result := 0;
    else
      result := natural( ceil( log2( real(val) ) ) );
    end if;
    return result;
  end;

  function max(val1 : integer; val2 : integer) return integer is
    variable result : integer;
  begin
    if val1 > val2 then
      result := val1;
    else
      result := val2;
    end if;
    return result;
  end;

  function min(val1 : integer; val2 : integer) return integer is
    variable result : integer;
  begin
    if val1 < val2 then
      result := val1;
    else
      result := val2;
    end if;
    return result;
  end;

  function umax(val1 : unsigned; val2 : unsigned) return unsigned is
    variable result : unsigned(max(val1'length, val2'length)-1 downto 0);
  begin
    if val1 > val2 then
      result := val1;
    else
      result := val2;
    end if;
    return result;
  end;

  function umin(val1 : unsigned; val2 : unsigned) return unsigned is
    variable result : unsigned(max(val1'length, val2'length)-1 downto 0);
  begin
    if val1 < val2 then
      result := val1;
    else
      result := val2;
    end if;
    return result;
  end;
  
  procedure pauseSignal( signal sig : inout std_logic; period : in time; cond : in boolean) is
  begin
	 if cond then
		sig	<= not(sig);
		wait for period;
		sig	<= not(sig);
	 end if;
  end procedure;

  procedure config_to_vecs(		
							signal conf 				: in t_config;
							signal aadr, badr, cadr : out std_logic_vector(awidth-1 downto 0); 
							signal aoSz,aiSz,boSz   : out unsigned(15 downto 0) ) is
  begin
	 aadr	<= conf.A_Address;
	 badr	<= conf.B_Address;
	 cadr	<= conf.C_Address;
	 aoSz	<= conf.A_outerSz;
	 aiSz <= conf.A_innerSz;
	 boSz <= conf.B_outerSz;
  end procedure;

  function t_status_to_slv(L: t_status) return std_logic_vector is 
    variable RetVal:    std_logic_vector(15 downto 0); 
    begin
        RetVal(L.reserved'range)		:= L.reserved; 
        RetVal(L.error'range)       := L.error; 
        RetVal(L.error_size'range)  := L.error_size; 
        RetVal(L.busy'range)      	:= L.busy; 
        return(RetVal); 
  end function t_status_to_slv; 

  function slv_to_t_status(L: std_logic_vector(15 downto 0)) return t_status is 
    variable RetVal:    t_status; 
    begin 
        RetVal := 
        ( 
            reserved   	=> L(RetVal.reserved'range),
				error	      => L(RetVal.error'range),
				error_size	=> L(RetVal.error_size'range),
            busy       	=> L(RetVal.busy'range)
        ); 
        return(RetVal); 
  end function slv_to_t_status; 

  function slv_to_t_command(L: std_logic_vector(15 downto 0)) return t_command is 
    variable RetVal:    t_command; 
    begin 
        RetVal := 
        ( 
            reserved   	=> L(RetVal.reserved'range),
            operation  	=> L(RetVal.operation'range),
				clear_err	=> L(RetVal.clear_err'range),
            run       	=> L(RetVal.run'range)
        ); 
        return(RetVal); 
  end function slv_to_t_command; 

  function slv_to_t_config(L: std_logic_vector(3*awidth+47 downto 0)) return t_config is 
    variable RetVal:    t_config; 
    begin 
        RetVal := 
        ( 
				A_Address   	=> L(RetVal.A_Address'range), 
            B_Address   	=> L(RetVal.B_Address'range), 
            C_Address   	=> L(RetVal.C_Address'range), 
            A_outerSz   	=> unsigned(L(RetVal.A_outerSz'range)), 
            A_innerSz   	=> unsigned(L(RetVal.A_innerSz'range)), 
            B_outerSz      => unsigned(L(RetVal.B_outerSz'range))
        ); 
        return(RetVal); 
  end function slv_to_t_config;
  
  function ternu(cond : boolean; res_true, res_false : unsigned) return unsigned is
  begin
	 if cond then
		return res_true;
	 else
	   return res_false;
	 end if;
  end function;
  function terni(cond : boolean; res_true, res_false : integer) return integer is
  begin
	 if cond then
		return res_true;
	 else
	   return res_false;
	 end if;
  end function;
  
  function rr(L: std_logic_vector) return std_logic_vector is
   -- A Shorthand for Range Rebasing
   -- (102 downto 100), will be returned as (2 downto 0)
    variable result : std_logic_vector(L'length-1 downto 0);
  begin
    for i in result'range loop
     result(i) := L(L'low+i);
   end loop;
    return result;
  end;
  
  function umult_same( a, b : unsigned) return unsigned is
    variable mylong  : unsigned(a'length + b'length -1 downto 0);
    variable myshort : unsigned(max(a'length, b'length) -1 downto 0);
  begin
    mylong	:= a*b;
	 myshort	:= mylong(myshort'range);
	 return myshort;
  end;
  
  function uceildiv( a, b : unsigned) return unsigned is
    variable result : unsigned(max(a'length, b'length) -1 downto 0);
  begin
    result := (a + b -1)/b;
    return result;
  end;

--
end ai_util_pkg;