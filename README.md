MoneyStacks
===========
Moneystacks is a personal money management tool.
The idea is based around the concept of money stacks for different
purposes and contexts (as you would stack money up in the real world).

I like to think of it as a "virtual money partition manager":
When bank accounts and physical locations of money are hard drives, then
moneystacks is the LVM on top, assigning money to arbitrary virtual volumes.

Usage
-----
moneystacks reads a configuration file <b>(see the example file for
concrete syntax information)</b> and generates a stream of transactions,
which are then applied to simulate/reconstruct the flow of money from,
to and between your stacks.

Many commands use the implicit "main" stack,
which means "unallocated money", and when source/destination stacks
are omitted, it is assumed that the money is transferred from/to the
"outside world".

You have to set an Origin date (day zero for all calculations) and
probably want to initialize your stack(s) with some values.

After that you might like to add macros for all your Regular incomes
and spendings, because you don't want to add recurring transactions
by hand each time, as I assume you are lazy like me.

Now you probably want to establish some additional stacks. There are
two flavors supported:

The Saving macro simply transfers a fixed amount of money from the main
stack to a different stack monthly. This is straight forward. You want
to buy a new TV on the long term, then just put aside some money each month.

The Limit macro does the same, but instead of growing over time, at the
end of each month everything left will be returned to the main stack.
This type is useful to control your spendings for a specific category
within the last month, e.g. you can Limit a groceries stack and assign
all spendings for groceries to be taken from it.

Moneystacks has three functions:

1. moneystacks can show you a history of all transactions between two dates.

2. moneystacks can show you the amount of money of each stack for any given
date, which is it's purpose in the first place. You can then use the stack
values to reflect on your spending behaviour.

3. moneystacks gives you a command line interface to add a single
transaction to the configuration file. This is what you are assumed to use
to track your money flows aside of the regular stuff.

Limits
------
The simplicity of moneystacks may be also it's biggest drawback -
you are responsible for the simulation of money flows being correct.
You have to see for yourself what meaning you assign to your stacks and that
all Macros and Transactions really do approximate actions in the real world.
A stack is what you make it, you can have stacks for each bank account or
you can use the main stack for all unassigned money (as I intended it).

There is no support for floating point numbers, because all what moneystacks
is supposed to do for me is to approximate the moneyflows that happen in my
life and guide me in my spending behaviour.

And as it is a mere approximation, from time to time you might have to
add "correction transactions", if the macros and the real world drift apart.

Installation
------------
```
git clone https://github.com/apirogov/MoneyStacks.git
cd MoneyStacks
cabal install
```

**Build documentation:** `cabal haddock`

**Run test suite:** `cabal test --show-details=always`

Make also sure, that `~/.cabal/bin` is in your `$PATH`.
