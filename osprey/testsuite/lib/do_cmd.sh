#!/bin/bash
# If 1 argument was provided, take it as a bash command, return the execution
# result
if [ $# -eq 1 ]
then
  if ($1)
  then
    echo do_cmd_succeed
  else
    echo do_cmd_failed
  fi
fi

# If 2 arguments were provided, take $1 as a bash command, put the stdout and
# stderr in file $2, return the execution result
if [ $# -eq 2 ]
then
  if [ -n "$2" ]
  then
    if ($1 >& $2 )
    then
      echo do_cmd_succeed
    else
      echo do_cmd_failed
    fi
  else
    if ($1)
    then
      echo do_cmd_succeed
    else
      echo do_cmd_failed
    fi
  fi
fi

# If 3 arguments were provided, take $1 as a bash command, add the stdout and
# stderr to file $2, return the execution result
if [ $# -eq 3 ]
then
  if [ -n "$2" ]
  then
    if ($1 >> $2 2>> $2 )
    then
      echo do_cmd_succeed
    else
      echo do_cmd_failed
    fi
  else
    if ($1)
    then
      echo do_cmd_succeed
    else
      echo do_cmd_failed
    fi
  fi
fi
