package ru.misc

import com.cmcmarkets.storage.Storage
import twitter4j.auth.AccessToken
import twitter4j.*
import static com.cmcmarkets.beans.Bean.bean
import com.cmcmarkets.csv.CsvWriter

/**
 * @author DKandalov
 */
class GTwitter {

  private static final String CONSUMER_KEY = ""
  private static final String CONSUMER_SECRET = ""
  private static final String TOKEN = ""
  private static final String TOKEN_SECRET = ""

  public static void main(String[] args) {
    ResponseList<Status> tweets = Storage.cached("stats_tweets") {
      TwitterFactory tf = new TwitterFactory()
      Twitter twitter = tf.instance
      twitter.setOAuthConsumer(CONSUMER_KEY, CONSUMER_SECRET)
      twitter.OAuthAccessToken = new AccessToken(TOKEN, TOKEN_SECRET)

      int page = 1
      def lastResult = twitter.getUserTimeline(new Paging(page++))
      def tweets = lastResult
      while (!lastResult.isEmpty()) {
        lastResult = twitter.getUserTimeline(new Paging(page++))
        tweets += lastResult
        println page
      }
      tweets
    }
    println tweets.findAll{it.text.contains("#kata")}.size()
    println tweets.findAll{it.text.contains("#gym")}.size()

/*
    println "by calendar day:"
    CsvWriter.write("by-cal-day.csv", smByCalDay(tweets))
    println "by day:"
    CsvWriter.write("by-day.csv", smByDay(tweets))
*/

    CsvWriter.write("by-time.csv", smByTime(tweets))

    println tweets.findAll { it.text.contains("#sm") }.size()
    println tweets.min { it.getCreatedAt() }


    /*twitter.setOAuthConsumer(CONSUMER_KEY, CONSUMER_SECRET)
    def requestToken = twitter.getOAuthRequestToken()

    AccessToken accessToken = null;
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
    while (null == accessToken) {
      System.out.println("Open the following URL and grant access to your account:");
      System.out.println(requestToken.getAuthorizationURL());
      System.out.print("Enter the PIN(if aviailable) or just hit enter.[PIN]:");
      String pin = br.readLine();
      try {
        if (pin.length() > 0) {
          accessToken = twitter.getOAuthAccessToken(requestToken, pin);
        } else {
          accessToken = twitter.getOAuthAccessToken();
        }
      } catch (TwitterException te) {
        if (401 == te.getStatusCode()) {
          System.out.println("Unable to get the access token.");
        } else {
          te.printStackTrace();
        }
      }
    }
    println accessToken.token
    println accessToken.tokenSecret

    println(twitter.updateStatus("hi!!!!"))*/
  }

  static List smByTime(ResponseList tweets) {
    tweets.collect { bean(time: it.createdAt, value: it.text.count("#sm")) }.findAll {it.value > 0}
  }

  private static List smByCalDay(ResponseList tweets) {
    return tweets.groupBy { "${it.createdAt.date}/${it.createdAt.month + 1}" }.entrySet().collect { entry ->
      def aSum = entry.value.collect {it.text.count("#sm")}.sum()
      bean([day: entry.key, value: aSum])
    }
  }

  private static List smByDay(ResponseList tweets) {
    int lastCount = Integer.MAX_VALUE
    int count = 0
    def lastDay = null
    def byDay = [:]
    tweets.sort {it.createdAt}.each { twit ->
      if (twit.text.contains("#sm")) {
        def match = twit.text =~ /(\d+).*/
        if (match.count == 1) {
          count = match[0][1].toInteger()
          if (count < lastCount) {
            if (lastDay != null) {
              byDay[lastDay] = lastCount
            }
            lastDay = twit.createdAt
          }
          lastCount = count
        }
      }
    }
    byDay = byDay.collect {bean([date: it.key, value: it.value])}
    return byDay
  }
}
