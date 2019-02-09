from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait #different things of selenium, that we need
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException
from selenium.common.exceptions import WebDriverException
import time
while 1: #main external cycle of program
    query = input("Enter your query or /help to see the list of commands\n")
    if query == "/help":
        print(" /exchange - convert dollars to rubles in online change rates\n /weather - see current temperature in city\n /exit - quit the program")
    elif query == "/exchange":
        while 1: #cycle, when unsuccesful
            try:
                amount = float(input("Input the amount of american dollars\n"))
                break
            except ValueError:
                print("Enter he sum with integer number, or in decimal like 1.75")
        #work with google information
        try:
            driver = webdriver.Firefox()
        except WebDriverException as err: #if there is no Firefox path on machine
            print("Problems with your browser\n",err)
            break
        driver.get("https://www.google.com/search?q=%D0%BA%D1%83%D1%80%D1%81+%D0%B4%D0%BE%D0%BB%D0%BB%D0%B0%D1%80%D0%B0&ie=utf-8&oe=utf-8&client=firefox-b-ab")
        t = driver.find_element_by_id("knowledge-currency__tgt-amount") #unique element in html code of this web-page
        s = str(t.text [0]) + str(t.text [1]) + '.' + str(t.text [3]) + str(t.text [4])                      
        dollar_rate = float(s)
        print("In russian rubles: ",dollar_rate*amount)
        driver.close()
    elif query == "/weather":
        while 1: #cycle, when unsuccesful
            city = input("Enter the city, which weather you want to know\n")
            #making our search of weather
            try:
                driver = webdriver.Firefox()
            except WebDriverException as err:
                print("Problems with your browser\n",err)
                break 
            driver.get("https://www.google.com")
            searchbox = driver.find_element_by_name("q")
            searchbox.send_keys("weather in ")
            searchbox.send_keys(city)
            searchbox.send_keys(Keys.RETURN)
            delay = 5
            try: #waiting for element for 5 seconds
                temp = WebDriverWait(driver, delay).until(EC.presence_of_element_located((By.ID, 'wob_tm')))
                print(temp.text,"°C")
                break
            except TimeoutException: #if there is no weather block in google for this query
                print("I can't find it, again...", end='')
            finally:
                driver.close()
    elif query == "/exit":
        break
    else:
        print("I don't understand, what you want...", end = '')
#something like animation
print("closing")
time.sleep(0.2)
print("closing.")
time.sleep(0.2)
print("closing..")
time.sleep(0.2)
print("closing...")
time.sleep(0.2)
