# encoding: utf-8

require 'csv'
require 'mechanize'
require 'date'


kodsall = *(1..435)
kods = kodsall.delete_if { |ko| [245,317,401,407].include? ko }
bdate = if ARGV[0] 
   ARGV[0]
 else 
   Date.today.strftime("%d.%m.%Y")
end
edate = if ARGV[1] 
   ARGV[1]
 else 
   Date.today.strftime("%d.%m.%Y")
end

CSV.open("#{bdate}_#{edate}_el.csv", "ab") do |data|
  header = Array.new
  header[0] = "MP name"
  header[1] = "date"
  header[2] = "time"
  header[3] = "time of day"
  header[4] = "registration"
  data << header
end

arr = []

kods.each do |kod|
  agent = Mechanize.new
  begin
  page = agent.get("http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_dep_reg_list_print?startDate="+bdate+"&endDate="+edate+"&kod="+kod.to_s)
  arr[0] = page.search('tr')[1].text.split(/\n/)[1]
  if page.search('tr')[3..-1]!=nil
    page.search('tr')[3..-1].each do |tr|
      unless tr.children.count ==1
        a = tr.text.split(/\n/) 
        arr[1] = a[2].split("  ")[0]
        arr[2] = a[2].split("  ")[1]
        arr[3] = a[4]
        arr[4] = a.last
          CSV.open("#{bdate}_#{edate}_el.csv", "ab", {:col_sep => "\t"}) do |data|
          data << arr
        end
      end
    end 
  end
  puts arr[0]
  sleep 1
  rescue
    next
  end
end
