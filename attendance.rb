# encoding: utf-8

require 'csv'
require 'mechanize'
require 'date'


kodsall = *(2..486)
kods = kodsall.delete_if { |ko| [397, 230, 287, 220, 231, 228, 309, 265, 222, 313, 332, 418, 327, 312, 132, 440, 422, 356, 310, 341, 336, 435, 84, 427, 292, 462, 466].include? ko }
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

arr = []

kods.each do |kod|
  agent = Mechanize.new
  page = agent.get("http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_dep_reg_w_list_print?startDate="+bdate+"&endDate="+edate+"&kod="+kod.to_s)
  arr[0] = page.search('tr')[1].text.split(/\n/)[1]
  if page.search('tr')[3..-1]!=nil
    page.search('tr')[3..-1].each do |tr|
      a = tr.text.split(/\n/)
      arr[1] = a[2]
      arr[2] = a[4]
      arr[3] = a[7]
        CSV.open("register.csv", "ab", {:col_sep => "\t"}) do |data|
        data << arr
      end
    end 

  end
  sleep 1
end


