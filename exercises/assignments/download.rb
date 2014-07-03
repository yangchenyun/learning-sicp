require 'pry'
require 'mechanize'
require 'uri'

def folder_name(link)
  link.text.lstrip.downcase.gsub(/[^\w]/, '-')
end

def href(link)
  link.attr(:href)
end

def intro_text(page)
  text = page.search('blockquote').text
  i = text.index('Files for download')
  text[0...i].sub(/^\n+/, '').gsub(/\n\n+/, "\n\n")
end

def write_file(name, content)
  file = File.open(name, 'w')
  file.write content
  file.close
end

agent = Mechanize.new
page = agent.get 'http://mitpress.mit.edu/sicp/psets/'
links = page.search('table tr td a')

links.each_with_index do |link, i|
  folder = "#{i + 1}.".rjust(3, '0') + folder_name(link)
  `mkdir -p #{folder}`
  Dir.chdir(folder) do
    subpage = agent.click link
    puts "click #{subpage.uri}"
    write_file 'intro.txt', intro_text(subpage)
    subpage.search('table a').each do |flink|
      result = agent.click flink
      write_file result.filename, result.content
      agent.back
    end
    puts "back from #{subpage.uri}"
    agent.back
  end
end
