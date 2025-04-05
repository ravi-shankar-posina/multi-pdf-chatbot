// responseData.js
export const categoryResponses = {
    access: {
      xmlResponse: `
        <supportResponse>
          <query>I can't access my account</query>
          <triageAgent>
            <mlPrediction>Access Issue</mlPrediction>
            <nlpPrediction>Account Access Problems</nlpPrediction>
            <reasoning>The query explicitly mentions access problems, which directly maps to the 'Access Issue' category based on our classification system.</reasoning>
            <category>Access/Authorization issue</category>
            <subcategory>Existing User</subcategory>
          </triageAgent>
          <nextAgent>
            <recommendedAgent>Access Specialist</recommendedAgent>
            <availability>Available Now</availability>
            <nextStep>Verify account details and check for temporary outages or account locks due to multiple failed login attempts. Run account verification process to restore access.</nextStep>
          </nextAgent>
          <analysis>
            <sentiment>frustrated</sentiment>
            <priority>high</priority>
            <category>account access</category>
            <keywords>
              <keyword>access</keyword>
              <keyword>account</keyword>
              <keyword>login</keyword>
              <keyword>can't</keyword>
            </keywords>
          </analysis>
          <suggestions>
            <suggestion>Check if user is entering correct username</suggestion>
            <suggestion>Verify if account has been locked due to multiple failed attempts</suggestion>
            <suggestion>Check for any recent system outages</suggestion>
            <suggestion>Suggest trying a different browser or clearing cache</suggestion>
          </suggestions>
          <externalLinks>
            <link title="Account Access Troubleshooting" url="https://support.example.com/account-access" />
            <link title="System Status Page" url="https://status.example.com" />
            <link title="Browser Cache Clearing Guide" url="https://support.example.com/clear-cache" />
          </externalLinks>
          <response>I understand you're having trouble accessing your account. This could be due to several reasons: incorrect login credentials, temporary account lock after multiple failed attempts, or system issues. First, verify you're using the correct username and password. If you're sure they're correct, your account may be temporarily locked - wait 30 minutes and try again. Also, try clearing your browser cache or using a different browser. Our system status page shows no current outages affecting logins. Would you like me to guide you through our account recovery process?</response>
        </supportResponse>
      `,
      loadingMessages: {
        triageAgent: "Running: access_verification_tool...",
        nextAgent: "Running: account_status_check...",
        analysis: "Analyzing access patterns...",
        suggestions: "Generating access troubleshooting steps...",
        externalLinks: "Searching knowledge base for access solutions...",
        response: "Triggering Access Specialist Agent..."
      }
    },
    
    authorize: {
      xmlResponse: `
        <supportResponse>
          <query>I need authorization for the admin panel</query>
          <triageAgent>
            <mlPrediction>Permission Request</mlPrediction>
            <nlpPrediction>Authorization Elevation</nlpPrediction>
            <reasoning>The query involves requesting higher access privileges to an administrative interface, which aligns with authorization elevation requests in our taxonomy.</reasoning>
            <category>Access/Authorization issue</category>
            <subcategory>Permission Elevation</subcategory>
          </triageAgent>
          <nextAgent>
            <recommendedAgent>Security Administrator</recommendedAgent>
            <availability>Available in 10 minutes</availability>
            <nextStep>Verify user identity, check eligibility for admin access based on role, document justification for elevated access, and submit to authorization workflow system.</nextStep>
          </nextAgent>
          <analysis>
            <sentiment>neutral</sentiment>
            <priority>medium</priority>
            <category>authorization</category>
            <keywords>
              <keyword>authorization</keyword>
              <keyword>admin</keyword>
              <keyword>panel</keyword>
              <keyword>access</keyword>
            </keywords>
          </analysis>
          <suggestions>
            <suggestion>Verify user identity with manager approval</suggestion>
            <suggestion>Check user role in organization chart</suggestion>
            <suggestion>Document business justification for admin access</suggestion>
            <suggestion>Specify time period for elevated access</suggestion>
          </suggestions>
          <externalLinks>
            <link title="Admin Access Request Form" url="https://intranet.example.com/admin-request" />
            <link title="Authorization Policy Document" url="https://policies.example.com/authorization" />
            <link title="Role-Based Access Control Guide" url="https://support.example.com/rbac" />
          </externalLinks>
          <response>To request authorization for the admin panel, we'll need to follow our company's authorization protocol. First, I'll need your manager's approval - please have them email security@example.com confirming your need for access. Second, please fill out our Admin Access Request Form detailing your specific business justification and the duration needed. Once submitted, our security team will review your request within 1-2 business days. For urgent requests, please call our security hotline at extension 5555. Note that admin access is granted temporarily (usually 30-90 days) and requires a clean security record.</response>
        </supportResponse>
      `,
      loadingMessages: {
        triageAgent: "Running: authorization_eligibility_tool...",
        nextAgent: "Running: security_clearance_check...",
        analysis: "Analyzing authorization requirements...",
        suggestions: "Generating authorization procedure steps...",
        externalLinks: "Searching security policies database...",
        response: "Triggering Security Administrator Agent..."
      }
    },
    
    forget: {
      xmlResponse: `
        <supportResponse>
          <query>I forgot my password</query>
          <triageAgent>
            <mlPrediction>Pwd Reset</mlPrediction>
            <nlpPrediction>Password Recovery</nlpPrediction>
            <reasoning>The query directly states a forgotten password, which maps to our 'Password Reset' category with high confidence.</reasoning>
            <category>Access/Authorization issue</category>
            <subcategory>Password Recovery</subcategory>
          </triageAgent>
          <nextAgent>
            <recommendedAgent>Password Recovery Specialist</recommendedAgent>
            <availability>Available Now</availability>
            <nextStep>Guide user through self-service password reset flow or initiate identity verification process for assisted reset.</nextStep>
          </nextAgent>
          <analysis>
            <sentiment>neutral</sentiment>
            <priority>medium</priority>
            <category>password recovery</category>
            <keywords>
              <keyword>forgot</keyword>
              <keyword>password</keyword>
              <keyword>reset</keyword>
              <keyword>remember</keyword>
            </keywords>
          </analysis>
          <suggestions>
            <suggestion>Direct to self-service password reset portal</suggestion>
            <suggestion>Verify user has access to registered email</suggestion>
            <suggestion>Check for account recovery options</suggestion>
            <suggestion>Suggest password manager for future use</suggestion>
          </suggestions>
          <externalLinks>
            <link title="Password Reset Portal" url="https://accounts.example.com/reset" />
            <link title="Account Recovery Options" url="https://support.example.com/account-recovery" />
            <link title="Password Manager Solutions" url="https://security.example.com/password-managers" />
          </externalLinks>
          <response>To reset your forgotten password, please visit our Password Reset Portal at accounts.example.com/reset. You'll need access to the email address registered with your account. After entering your username or email, we'll send you a secure link that expires in 30 minutes. Click the link and follow the instructions to create a new password. Remember to choose a strong password you haven't used before. If you don't receive the email within 5 minutes, please check your spam folder. For additional security, consider using a password manager to help create and store strong, unique passwords.</response>
        </supportResponse>
      `,
      loadingMessages: {
        triageAgent: "Running: password_reset_tool...",
        nextAgent: "Running: identity_verification_check...",
        analysis: "Analyzing account security status...",
        suggestions: "Generating password recovery options...",
        externalLinks: "Retrieving password reset resources...",
        response: "Triggering Password Recovery Agent..."
      }
    }
  };