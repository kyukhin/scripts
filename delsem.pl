#!/usr/bin/perl

# ******************************** main
{
    if ( @ARGV  )
    {
     	#usage 
        print "Usage:perl delsem.pl\n";
        exit 0;
    }

  
    open SEM_LIST, "ipcs -s |";  
    while ( <SEM_LIST> )
    {  	
       ($sem_id) = $_ =~ /0x\w+\s(\d+)\s/;
	   #print "$_ : $sem_id\n";
	   if ( $sem_id )
	   {
           system "ipcrm -s $sem_id &> /dev/null";   
	   }
    }
    close SEM_LIST;
} 