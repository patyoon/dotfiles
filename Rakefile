FILES = Hash.new
FILES[:git] = %w(.gitconfig .gitignore .gitignore_global)
FILES[:zsh] = %w(.zshrc .zprofile .zsh_aliases)
FILES[:python] = %w(.pylintrc .pythonstartup)
FILES[:misc] = %w(.screenrc .inputrc tmux.conf)
FILES[:emacs] = %w(patyoon.el)
FILES[:ruby] = %w(.gemrc)

PACKAGES = Hash.new
PACKAGES[:python] = 'python_packages.txt'
PACKAGES[:ruby] = 'Gemfile'

namespace 'install' do
  task ':all' => [:git, :zsh, :python, :emacs, :ruby] do
    puts 'Installed all!'
  end

  task 'git' do
    FILES[:git].each { |file| determine_action file }
  end

  task 'bash' do
    FILES[:bash].each { |file| determine_action file }
  end

  task 'python' do
    FILES[:python].each { |file| determine_action file }
  end
  task 'ruby' do
    FILES[:ruby].each { |file| determine_action file }
  end

  task 'emacs' do
    FILES[:emacs].each{ |file| determine_action file }
  end
end

def determine_action(file, dest=ENV['HOME'])
  if File.exists? "#{dest}/#{file}"
    if $replace_all
      replace_file file dest
    else
      loop do
        print "Overwrite ~/#{file}? [ynAq] "
        case STDIN.gets.chomp
        when 'A'
          $replace_all = true
          replace_file file dest
          break
        when 'y'
          replace_file file dest
          break
        when 'q'
          exit
        when 'n'
          puts "Skipping #{file}"
          break
        else
          puts "Invalid Input. Type one of ynAq"
        end
      end
    end
    link_file file dest
  end
end

def clone_omz
  omz_path = "#{ENV['HOME']}/.oh-my-zsh"
  repo_url = 'https://github.com/robbyrussell/oh-my-zsh.git'
  if File.exists? omz_path
    puts '    ~/.oh-my-zsh already exists, skipping'
    puts 'To reinstall OMZ, rename or remove ~/.oh-my-zsh and try again.'
    return
  end
  system "git clone #{repo_url} #{omz_path}"
  system "cd #{omz_path} && git submodule update --init --recursive"
end

def link_file(file, dest)
  puts "    linking #{dest}/#{file}"
  system "ln -s \"$PWD/#{file}\" \"#{dest}/#{file}\""
end

def replace_file(file, dest)
  puts "    removing old #{dest}/#{file}"
  system "rm -f \"#{dest}/#{file}\""
  link_file file
end
