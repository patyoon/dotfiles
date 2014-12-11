require 'mkmf'

FILES = Hash.new
FILES[:git] = %w(.gitconfig .gitattributes .gitconfig_aliases .gitignore_global)
FILES[:zsh] = %w(.zshrc .zprofile .zsh_aliases)
FILES[:python] = %w(.pylintrc .pythonstartup)
FILES[:misc] = %w(.screenrc .inputrc tmux.conf)
FILES[:emacs] = %w(patyoon.el)
FILES[:ruby] = %w(.gemrc .pryrc .rdebugrc)

PACKAGES = Hash.new
PACKAGES[:python] = 'python_packages.txt'
PACKAGES[:ruby] = 'Gemfile'
PACKAGES[:brew] = 'brew_packages.txt'

EMACS_PATH = "#{ENV['HOME']}/.emacs.d"
PRELUDE_REPO_URL = 'https://github.com/bbatsov/prelude.git'
PRELUDE_MODULE_FILE = 'prelude_modules.el'

OMZ_PATH = "#{ENV['HOME']}/.oh-my-zsh"
OMZ_REPO_URL = 'https://github.com/robbyrussell/oh-my-zsh.git'

BREW_INSTALL_CMD = 'ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"'
BREW_PACKAGE_FILE = 'brew_packages.txt'

PYTHON_PACKAGE_FILE = 'python_packages.txt'

namespace 'install' do
  task 'all' => [:git, :zsh, :python, :emacs, :ruby] do
    puts 'Installed all!'
  end

  task 'zsh' do
    clone_omz
    FILES[:zsh].each { |file| determine_action(file, "zsh") }
  end

  task 'git' do
    FILES[:git].each { |file| determine_action(file, "git") }
  end

  task 'python' do
    # TODO: Incorporate virtualenv.
    FILES[:python].each { |file| determine_action(file, "python") }
    print "Install all packages in #{PYTHON_PACKAGE_FILE}? [yn] "
    case STDIN.gets.chomp
    when 'y'
      print "Installing all packages in #{PYTHON_PACKAGE_FILE}"
      system "sudo pip install -U -r python/#{PYTHON_PACKAGE_FILE}"
    when 'n'
      exit
    else
      puts "Invalid Input. Type one of [yn]"
    end
  end

  task 'ruby' do
    FILES[:ruby].each { |file| determine_action(file, "ruby") }
    print "Install all packages in #{PYTHON_PACKAGE_FILE}? [yn] "
    case STDIN.gets.chomp
    when 'y'
      print "Installing all packages in #{PYTHON_PACKAGE_FILE}"
      system "sudo pip install -U -r #{PYTHON_PACKAGE_FILE}"
      break
    when 'n'
      exit
    else
      puts "Invalid Input. Type one of [yn]"
    end
  end

  task 'emacs' do
    clone_emacs_prelude
    # Add prelude_modules.el which contains modules to use.
    link_file(PRELUDE_MODULE_FILE, 'emacs', EMACS_PATH)
    # Link main el file.
    FILES[:emacs].each do |file| determine_action(
      file, "emacs", "#{EMACS_PATH}/personal")
    end
    # Link all partial el files in /partials.
    Dir.glob('emacs/partials/*').select do |file|
      puts File.basename(file)
      determine_action(File.basename(file), "emacs/partials", "#{EMACS_PATH}/personal/partials")
    end
  end

  task 'brew' do
    if !find_executable 'brew'
      system BREW_INSTALL_CMD
      print "Install all packages in #{BREW_PACKAGE_FILE}? [yn] "
      case STDIN.gets.chomp
      when 'y'
        system "cat brew/#{BREW_PACKAGE_FILE} | xrags brew install"
        break
      when 'n'
        exit
      else
        puts "Invalid Input. Type one of [yn]"
      end
    end
  end
end

namespace 'update' do
  task 'emacs' do
    if !File.directory?(EMACS_PATH)
      # Set up emacs.
      Rake::Task['install:emacs'].invoke
    else
      system "cd #{EMACS_PATH} && git submodule update --init --recursive"
    end
  end

  task 'brew' do
    if !find_executable 'brew'
      Rake::Task['install:brew'].invoke
    else
      system "brew update && brew upgrade `brew outdated`"
    end
  end

  task 'zsh' do
    if !File.directory?(OMZ_PATH)
      # Set up oh-my-zsh.
      Rake::Task['install:zsh'].invoke
    else
      system "cd #{OMZ_PATH} && git submodule update --init --recursive"
    end
  end
end

def determine_action(file, type, dest=ENV['HOME'])
  if File.exists? "#{dest}/#{file}"
    if $replace_all
      replace_file(file, type, dest)
    else
      print "Overwrite ~/#{file}? [ynAq] "
      case STDIN.gets.chomp
      when 'A'
        $replace_all = true
        replace_file(file, type, dest)
      when 'y'
        replace_file(file, type, dest)
      when 'q'
        exit
      when 'n'
          puts "Skipping #{file}"
      else
        puts "Invalid Input. Type one of [ynAq]"
      end
    end
  else
    link_file(file, type, dest)
  end
end

def print_help
  puts 'Usage: rake <task>'
  puts 'install:all\t-\tInstall all dotfiles'
  puts 'install:git\t-\tInstall git files'
  puts 'install:zsh\t-\tInstall zsh files'
  puts 'install:python\t-\tInstall python files'
  puts 'install:ruby\t-\tInstall ruby files'
  puts 'install:emacs\t-\tInstall emacs files and emacs prelude'
end

def clone_emacs_prelude
  if File.exists? EMACS_PATH
    puts '~/.emacs.d already exists.'
    puts 'To reinstall emacs, rename or remove ~/.emacs and try again.'
    return
  end
  system "git clone #{PRELUDE_REPO_URL} #{EMACS_PATH}"
  system "cd #{EMACS_PATH} && git submodule update --init --recursive"
end

def clone_omz
  if File.exists? OMZ_PATH
    puts '~/.oh-my-zsh already exists.'
    puts 'To reinstall OMZ, rename or remove ~/.oh-my-zsh and try again.'
  else
    system "git clone #{OMZ_REPO_URL} #{OMZ_PATH}"
    system "cd #{OMZ_PATH} && git submodule update --init --recursive"
  end
end

def link_file(file, type, dest_dir)
  # Extract directory part of dest and create if not exists.
  if !File.exists?(dest_dir)
    mkdir_p dest_dir
  end
  puts "linking #{dest_dir}/#{file}"
  system "ln -s -f $PWD/#{type}/#{file} #{dest_dir}/#{file}"
end

def replace_file(file, type, dest)
  puts "removing old #{dest}/#{file}"
  system "rm -f \"#{dest}/#{file}\""
  link_file(file, type, dest)
end
