# setup
rm(list = ls())

# libraries
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(readODS)

# load data
raw_data <- read_ods(here("Projects/webservers/web-server-comparison.ods"))

data <- raw_data %>% 
  clean_names() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2015)

# select data
data_to_plot <- data %>%
  gather(apache_httpd, express, traefik, caddy, nginx, 
         key = "server", 
         value = "number_of_vulnerabilities") %>%
  mutate(language = case_when(server == "apache_httpd" ~ "C",
                              server == "express" ~ "JS",
                              server == "traefik" ~ "Go",
                              server == "caddy" ~ "Go",
                              server == "nginx" ~ "C")) %>%
  mutate(memory_safety = case_when(language == "C" ~ "memory unsafe",
                                   language == "JS" ~ "memory safe",
                                   language == "Go" ~ "memory safe"))

data_to_plot %>% 
  group_by(server) %>%
  summarise(
    total = sum(number_of_vulnerabilities)
  ) %>%
  mutate(server = fct_reorder(server, total)) %>%
  mutate(language = case_when(server == "apache_httpd" ~ "C (memory unsafe)",
                              server == "express" ~ "JS (memory safe)",
                              server == "traefik" ~ "Go (memory safe)",
                              server == "caddy" ~ "Go (memory safe)",
                              server == "nginx" ~ "C (memory unsafe)")) %>%
  ggplot(aes(total, server, fill = language)) +
  geom_col() +
  theme_light() +
  labs(
    title = "Vulnerabilities in servers between 2015 and 2020",
    x = "Total number of vulnerabilities", 
    y = "Web server name"
  )









data_to_plot %>% 
  group_by(server) %>%
  summarise(
    total = sum(number_of_vulnerabilities)
  ) %>%
  mutate(server = fct_reorder(server, total)) %>%
  mutate(language = case_when(server == "apache_httpd" ~ "C (memory unsafe)",
                              server == "express" ~ "JS (memory safe)",
                              server == "traefik" ~ "Go (memory safe)",
                              server == "caddy" ~ "Go (memory safe)",
                              server == "nginx" ~ "C (memory unsafe)")) %>%
  ggplot(aes(total, server, fill = language)) +
  geom_col() +
  theme_light() +
  labs(
    title = "Vulnerabilities in servers between 2015 and 2020",
    x = "Total number of vulnerabilities", 
    y = "Web server name"
  )


data_to_plot %>% 
  group_by(server) %>%
  summarise(
    total = sum(number_of_vulnerabilities)
  ) %>%
  mutate(server = fct_reorder(server, total)) %>%
  mutate(language = case_when(server == "apache_httpd" ~ "C",
                              server == "express" ~ "JS",
                              server == "traefik" ~ "Go",
                              server == "caddy" ~ "Go",
                              server == "nginx" ~ "C")) %>%
  mutate(memory = case_when(language == "C" ~ "memory unsafe",
                            language == "JS" ~ "memory safe",
                            language == "Go" ~ "memory safe")) %>%
  ggplot(aes(total, server, fill = language)) +
  geom_col() +
  theme_light() +
  labs(
    title = "Vulnerabilities in servers between 2015 and 2020",
    x = "Total number of vulnerabilities", 
    y = "Web server name"
  )


data_to_plot %>% 
  group_by(memory) %>%
  summarise(
    total = sum(number_of_vulnerabilities)
  ) %>%
  ggplot(aes(total, memory)) +
  geom_col() +
  theme_light() +
  labs(
    title = "Vulnerabilities in servers between 2015 and 2020",
    x = "Total number of vulnerabilities", 
    y = "Web server name"
  )

data_to_plot %>%
  group_by(memory) %>%
  summarise(total =  sum(number_of_vulnerabilities)) %>%
  ggplot(aes(x=number_of_vulnerabilities, fill=language)) +
  geom_bar(colour = "black") +
  scale_fill_manual(name = "language", values=c("#f1faee","#a8dadc","#457b9d")) +
  labs(
    title = "Number of versions per year", 
    x = "Year", 
    y = "Number of versions"
  )

data_to_plot %>%
  ggplot(aes(memory_safety, sum(number_of_vulnerabilities)) +
  geom_bar()

  
data_to_plot %>% 
    group_by(memory_safety) %>%
    summarise(
      total = sum(number_of_vulnerabilities)
    ) %>%
    mutate(server = fct_reorder(server, total)) %>%
    mutate(language = case_when(server == "apache_httpd" ~ "C",
                                server == "express" ~ "JS",
                                server == "traefik" ~ "Go",
                                server == "caddy" ~ "Go",
                                server == "nginx" ~ "C")) %>%
    mutate(memory = case_when(language == "C" ~ "memory unsafe",
                              language == "JS" ~ "memory safe",
                              language == "Go" ~ "memory safe")) %>%
    ggplot(aes(total, server, fill = language)) +
    geom_col() +
    theme_light() +
    labs(
      title = "Vulnerabilities in servers between 2015 and 2020",
      x = "Total number of vulnerabilities", 
      y = "Web server name"
    )

data_to_plot %>%
  group_by(memory_safety) %>%
  summarise(total = sum(number_of_vulnerabilities)) %>%
  ggplot(aes(memory_safety, number_of_vulnerabilities)) +
  geom_bar()


ggplot(data_to_plot, 
       aes(x = memory_safety, 
           fill = number_of_vulnerabilities)) + 
  geom_bar(position = "stack")