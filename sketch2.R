term.plot(tStudent_identidade_x134,what = "mu",term = 5,
          col.shaded="royalblue",
          col.term = "black",
          xlabs="Log_neutra",col.se="red",ylabs="partial for cs(log_neutra)",
          lwd.term = 2,lwd.shaded = 2)

term.plot(tStudent_identidade_x134,what = "mu",term = 4,
          col.shaded="royalblue",
          col.term = "black",
          col.se="red",ylabs="partial for cs(log_neutra)",
          lwd.term = 2,lwd.shaded = 2)



term.plot(tStudent_identidade_x134,what = "mu",term = 3,
          col.shaded="royalblue",
          col.term = "black",
          xlabs="Log_neutra",col.se="red",ylabs="partial for cs(log_neutra)",
          lwd.term = 2,lwd.shaded = 2)


#plot with sexo
term.plot(tStudent_identidade_x134,what = "mu",term = 2,
          col.shaded="royalblue",
          col.term = "black",
          xlabs="Genero",col.se="red",ylabs="partial for sexo",
          lwd.term = 2,lwd.shaded = 2)


#plot with cs(x4)
term.plot(tStudent_identidade_x134,what = "mu",term = 1,
          col.shaded="royalblue",
          col.term = "black",
          xlabs="Vacuna",col.se="red",ylabs="partial for vacuna",
          lwd.term = 2,lwd.shaded = 2)
