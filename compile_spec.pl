#!/usr/bin/perl


$spec_path="~/usrad1/kyukhin/spec2000";
$gcc="gcc";

print "Compiling source...\n";
if( chdir($spec_path) )
{
  $old_path = `pwd`;
  print $old_path;
} else
{
  print "Spec src directory not found: $spec_path\n";
}
