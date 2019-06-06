---
title: Analytic Data Scientists -- know when to fold 'em
author: Richard Careaga
date: '2018-08-12'
slug: analytic-data-scientists-know-when-to-fold-em
categories:
  - Programming
tags:
  - debugging
---

In the days of our forebearers, if a tool broke, you were likely to fix it yourself. Long time passing, of course, the shadetree mechanic has been an endangered species. And no one would hire a carpenter who didn't own his own tools.

I've been one to use my own tools. I have to admit to being, at times, cavalier about corporate policies strictly forbidding the use of employee owned computers, at least in the building. (Why it was ok remotely without VPN is something of a mystery.)

I took Admiral Hopper's adage to heart

> It's easier to ask forgiveness than than it is to get permission.

Even with the overhead of being my own sysadm, I saved at least 90 minutes a week in boot time. The corporate XP system took (I kid you not) 20 minutes to boot and present a working GUI. In that time my Mac Pro could boot, connect to slow off-site WiFi and give me time to triage my morning email through the corporate webmail system.

However, as tools proliferate, the balance between overhead and maintenance has changed. 

TIL: The default Mac and Ubuntu compilers are C++11. Not C++14 nor C++17.

In a project to get literate in C++ I learned that the preferred way to declare a constant was

     const int age {25};
     
Since everything is up-to-date in Seattle, that's what I did, saddening the compiler and receiving a multitude of error messages. I hit upon the idea of good 'ol C style

    const int age = 25;
    
but I wondered what other slithery bugs awaited me.

Through the kindness of strangers, I learned of a compiler flag. I know, I know, RTFM. 

And then it struck me. I'm not a programmer. I never wanted to be a programmer. I just wanted to be able to read and understand a program in a language as important as C++ and maybe write some simple programs that gave a hemibel boost to execution.

As an analytic data scientist, I want to spend my time thinking about data and ways to extract useful information from them. I can't do that if I'm spending hours trying to figure out why syntactically valid software won't compile.

I'll be cultivating friendships with data engineers and programmers. 

