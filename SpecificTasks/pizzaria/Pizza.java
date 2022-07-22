//$PKGLINE
/**
 * I N F x 1 2 0
 *
 * DÈcrivez votre classe $CLASSNAME ici.
 *
 * @author (votre nom)
 * @version (une date)
 *
 * (votre code permanent)
 * (votre adresse de courriel)
 */

import java.util.*;

public class Pizza {
    private static String menu =
            "1. Invoice an order\n" +
            "2.display the total number of meals sold and the total amount of sales\n" +
            "3.exiting the program\n";
    private static String mealCart =
            "+-------------------------------------------------------------------------------------------------+\n"+
            "|                                  Le tabeau de repas(Super Combos)                               |\n" +
            "+----------+--------------------------------------------------------------------+-----------------+\n" +
            "|Le numéro | La description                                                     | Le prix unitaire|\n" +
            "|----------+--------------------------------------------------------------------+-----------------|\n" +
            "|     3    | 1 Moyenne Pizza 12\" + 1 Petite Poutine + 1 Pepsi(355ml)            |       20.49     |\n" +
            "|     4    | 1 Moyenne Pizza 12\" + 10 ailes de poulet + Frites + 1 Pepsi(355ml) |       21.99     |\n" +
            "|     8    | 1 Pizza Large 14\" + Moyenne Poutine + 1 Pepsi(355ml)               |       24.49     |\n" +
            "|     9    | 1 Pizza Large 14\" + 12 ailes de poulet + Frites + 1 Pepsi(355ml)   |       25.99     |\n" +
            "|    13    | 1 Pizza X-Large 16\" + 1 Grande Poutine + 1 Pepsi(355ml)            |       28.49     |\n" +
            "|    14    | 1 Pizza X-Large 16\" + 15 ailes de poulet + 1 Pepsi(355ml)          |       29.99     |\n" +
            "+-------------------------------------------------------------------------------------------------+\n";
    private static int[] numbers = {3,4,8,9,13,14}; //menu numbers
    private static double[] prices = {20.49,21.99,24.49,25.99,28.49,29.99}; //menu prices, each price has the same index of the meal number it mach in numbers array
    private static String [] pizzaType = {"V","F","T","E","M"};
    private static String [] paymentType = {"D","C","R"};
    private static int maxMeals = 100;
    private static String restaurantName = "Pizza Pour Tous";
    private static String restaurantAddress = "5252 Matata Street, Hakuna, QC";
    private static String restaurantNumber = "514 848-7733";

    //this method is used to read a integer from stdin, can be used to read a int of the main menu (if min = 1,max = 3,mealNumber = false)
    // or to read a meal number(mealNumber = true) or phone number (if min = 100000000,max = 999999999,mealNumber = false)
    //the received message will appear until the inserted int is valid, the program stop if a letter is inserted
    public static int ReadInt(String message, int min, int max,boolean mealNumber){
        Scanner s = new Scanner(System.in);
        int n = -1;

        do{
            System.out.println(message);
            try {
                String line = s.nextLine();
                n = Integer.parseInt(line);
            } catch (NumberFormatException nfe) {
                System.out.println("Invalid number!");
                System.exit(0);
            }
        } while ((mealNumber && containsInt(n) == -1)  || (!mealNumber && (n < min || n > max) ));

        return n;
    }

    //this method is used to valid a meal number
    //returns the index in the array, -1 if not exists
    public static int containsInt(int n){
        int i = 0,index = -1;

        while (i < 6 && index == -1){
            if(numbers[i] == n)
                index = i;
            i++;
        }
        return index;
    }

    //this method is used to valid a pizzaType or payment type
    //if pizza = true then check if String s is on array pizzaType
    //if pizza = false check on array paymentType
    public static boolean contains(String s,boolean pizza){
        int i = 0, max = 5;
        boolean contains = false;
        if(!pizza) max = 3;

        while (i < max && !contains){
            if((pizza && pizzaType[i].equals(s)) || (!pizza && paymentType[i].equals(s)))
                contains = true;
            i++;
        }
        return contains;
    }

    //this method is used to read a string from stdin, can be used to read any string(if pizza = false,payment = false)
    // or to read a pizza type(pizza = true) or a payment type (payment = true)
    //the received message will appear until the string readed is valid
    public static String ReadString(String message,boolean pizza,boolean payment){
        Scanner s = new Scanner(System.in);
        String line;

        do {
            System.out.println(message);
            line = s.nextLine();
        } while ((pizza && !contains(line.toUpperCase(),true)) || (payment && !contains(line,false)) || (pizza && payment));

        return line;
    }

    //option 1
    //this method is used to print the invoice, receive the client data, an array with the meal numbers he asked for(mealNumber[])
    // and other array (mealType) with the description of each meal
    //total price is calculed here too
    public static void invoice(int invoiceNr,String address,int phone,int [] mealNumber,String[] mealType,int mealSize,String payment) {
        double subtotal = 0;
        int i = 0;
        double price = 0;

        System.out.println(
                "+--------------------------------------------------------------+\n" +
                "|                       " + restaurantName.toUpperCase() + "                        |\n" +
                "+--------------------------------------------------------------+" +
                "\nDate:" + java.time.LocalDateTime.now() +
                "\nRestaurante details:                     Client details:\n"+
                restaurantAddress + "             "+address+
                "\n"+restaurantNumber + "                               "+phone+
                "\n\nInvoice number: " + invoiceNr+
                "\n+-------------+----------------------+--------+" +
                "\n| Meal number |      Description     |  Price |"+
                "\n+-------------+----------------------+--------+");

        while (i < mealSize) {
            price = prices[containsInt(mealNumber[i])];
            System.out.println("|      " + mealNumber[i] + "      |      " + mealDescription(mealType[i]) + "     |  " + price + " |");
            i++;subtotal += price;
        }
        double tps = subtotal * 0.05,tvq = subtotal * 0.09975,total = subtotal + tps + tvq;

        System.out.println("|-------------+----------------------+--------+" +
                "\n                            SubTotal: " + subtotal +
                "\n                                 TPS: " + tps +
                "\n                                 TVQ: " + tvq +
                "\n                               Total: " + total +
                "\nPayment method: " + paymentType(payment) +
                "\nThank you and see you next time!\n");
    }

    //option 2
    //this method is used to print the option2, receive the total sales and total meals
    //print the restaurante info, current date and total sales/meals
    public static void option2(int totalSales,int totalMeals) {
        System.out.println("+--------------------------------------------------------------+\n" +
                "|                       " + restaurantName.toUpperCase() + "                        |\n" +
                "+--------------------------------------------------------------+" +
                "\n| " + restaurantAddress + "     " + java.time.LocalDateTime.now() +
                "|\n| " + restaurantNumber + "                                                 |\n" +
                "|                                                              |\n" +
                "| Total Sales: " + totalSales + "                                               |\n" +
                "| Total meals: " + totalMeals + "                                               |\n" +
                "+--------------------------------------------------------------+\n");
    }

    //math the meal type char with its description
    public static String mealDescription(String meal){
        String description;
        switch (meal.toUpperCase()){
            case "V":
                description = "Vegetarian ";
                break;
            case "F":
                description = "Cheese     ";
                break;
            case "T":
                description = "Any topping";
                break;
            case "E":
                description = "Spinnach   ";
                break;
            case "M":
                description = "Seafood    ";
                break;
            default:
                description = "Undefined  ";
                break;
        }
        return description;
    }

    //math the payment type char with its description
    public static String paymentType(String type){
        String description;
        switch (type){
            case "C":
                description = "Cash     ";
                break;
            case "D":
                description = "Debit    ";
                break;
            case "R":
                description = "Credit   ";
                break;
            default:
                description = "Undefined";
                break;
        }
        return description;
    }

    //main method
    public static void main (String[] params) {
        int totalSales = 0;
        int totalMeals = 0;
        int option = 0;
        boolean exit = false;
        String yesORno;

        while (option != 3){
            option = ReadInt(menu,1,3,false);
            switch (option) {
                case 1:
                    exit = false;
                    totalSales++;
                    int phone = ReadInt("Phone number:",100000000,999999999,false);
                    String address = ReadString("Address:",false,false);
                    int[] mealNumber = new int[maxMeals];
                    String[] pizzaType = new String[maxMeals];
                    int mealSize = 0;

                    //read meal orders until yesORno string is different of "Y" or "y"
                    while (!exit) {
                        System.out.println(mealCart);
                        mealNumber[mealSize] = ReadInt("Meal Number:",0,0,true);
                        pizzaType[mealSize] = ReadString("Pizza Type  (v or V = Vegetarian, f or F = Cheese, t or T = Any topping, e or E = Spinach, m or M = Seafood):"
                                                            ,true,false);
                        mealSize++;
                        totalMeals++;
                        yesORno = ReadString("Add new meal (Y or y to yes):",false,false);
                        if(!yesORno.toUpperCase().equals("Y"))
                            exit = true;
                    }
                    String payment = ReadString("Payment type (C = Cash, D = Debit, and R = Credit)",false,true);
                    invoice(totalSales + 1,address,phone,mealNumber,pizzaType,mealSize,payment);
                    break;
                case 2:
                    option2(totalSales,totalMeals);
                    break;
                case 3:
                    System.out.println("Thank you and see you next time!");
                    break;
                default: break;
            }
        }
    } // main
}