execute "xmonad-compile" do
  command "/usr/bin/xmonad --recompile"
  creates "#{ENV['HOME']}/.xmonad/xmonad.o"
  action :nothing
end

link "#{ENV['HOME']}/.xmonad" do
  to "#{ENV['PWD']}/.."
  notifies :run, resources(:execute => "xmonad-compile")
end

directory "#{ENV['HOME']}/local/bin" do
  recursive true
end

Dir.glob(File.join(ENV['PWD'], "..", "bin", "*")).each do |f|
  link "#{ENV['HOME']}/local/bin/#{File.basename f}" do
    to f
  end
end

link "#{ENV['HOME']}/.gnomerc" do
  to "#{ENV['PWD']}/../dot-gnomerc"
end

link "#{ENV['HOME']}/.Xsession" do
  to "#{ENV['PWD']}/../dot-Xsession"
end

directory "#{ENV['HOME']}/.config/gnome-session/sessions" do
  recursive true
end

link "#{ENV['HOME']}/.config/gnome-session/sessions/xmonad-gnome-nopanel.session" do
  to "#{ENV['PWD']}/../xmonad-gnome-nopanel.session"
end
