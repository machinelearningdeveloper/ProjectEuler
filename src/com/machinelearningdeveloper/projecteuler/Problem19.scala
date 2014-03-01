package com.machinelearningdeveloper.projecteuler

/** Problem 19 http://projecteuler.net/problem=19
  * "You are given the following information, but you may prefer to do some research for yourself.
  * 
  *   "1 Jan 1900 was a Monday.
  *   Thirty days has September, April, June and November.
  *   All the rest have thirty-one,
  *   Saving February alone,
  *   Which has twenty-eight, rain or shine.
  *   And on leap years, twenty-nine.
  *   
  * "A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
  * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?"
  */

sealed trait DayOfWeek
case object Sunday extends DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek

sealed trait Month
case object January extends Month
case object February extends Month
case object March extends Month
case object April extends Month
case object May extends Month
case object June extends Month
case object July extends Month
case object August extends Month
case object September extends Month
case object October extends Month
case object November extends Month
case object December extends Month

object Problem19 extends App {
  val daysOfWeek = Vector(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
  
  println(numberOfDays(1900, 1901, 2000, Monday, Sunday, 1))
  
  def numberOfDays(startYear: Int, countStartYear: Int, endYear: Int, startingDay: DayOfWeek, dayOfWeekToCount: DayOfWeek, dayOfMonthToCount: Int): Int = {
    @scala.annotation.tailrec
    def count(dayOfWeek: DayOfWeek, dayOfMonth: Int, month: Month, year: Int, countOfDays: Int): Int = {
      val newCount = countOfDays +
        (if (dayOfWeek == dayOfWeekToCount && dayOfMonth == dayOfMonthToCount && year >= countStartYear) 1 else 0)
      if (dayOfMonth == 31 && month == December && year == endYear)
        newCount
      else
        count(nextDayOfWeek(dayOfWeek),
              nextDayOfMonth(dayOfMonth, month, year),
              nextMonth(dayOfMonth, month, year),
              nextYear(dayOfMonth, month, year),
              newCount)
    }
    count(startingDay, 1, January, startYear, 0)
  }
  
  def nextDayOfWeek(dayOfWeek: DayOfWeek) = dayOfWeek match {
    case Saturday => Sunday
    case _ => daysOfWeek(daysOfWeek.indexOf(dayOfWeek) + 1)
  }

  def nextDayOfMonth(dayOfMonth: Int, month: Month, year: Int) =
    if (dayOfMonth == monthLength(month, year))
      1
    else
      dayOfMonth + 1
    
  def nextMonth(dayOfMonth: Int, month: Month, year: Int) =
    if (dayOfMonth == monthLength(month, year))
      incrementMonth(month)
    else
      month
      
  def nextYear(dayOfMonth: Int, month: Month, year: Int) =
    if (dayOfMonth == 31 && month == December) year + 1 else year
    
  def monthLength(month: Month, year: Int) =
    if (month == February)
      februaryLength(year)
    else if (month == April || month == June || month == September || month == November)
      30
    else
      31
    
  def februaryLength(year: Int) =
    if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) 29 else 28
    
  def incrementMonth(month: Month) = {
    val months = Vector(January, February, March, April, May, June, July, August, September, October, November, December)
    val currentMonthIndex = months.indexOf(month)
    if (currentMonthIndex + 1 < months.length)
      months(currentMonthIndex + 1)  // Case next month > January
    else
      months(0)  // Case next month == January
  }
}